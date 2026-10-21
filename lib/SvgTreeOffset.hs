{-# LANGUAGE CPP #-}


module SvgTreeOffset (pathOutline) where

import Data.Either (rights)
import Data.List (foldl')
import Debug.Trace (trace)
import Graphics.Svg.Types (Coord, Origin (..), PathCommand (..), RPoint)
import Linear (V2 (..), (*^), (^*), (^/))
import SvgTreeAntiquote (Supplement (..), WArg (..))

-- | Turn a path with @w@ and @W@ commands into a closed stroke outline.
--
-- A @W@ argument sets absolute radii; @w@ adds to the current radii. A value
-- is one radius for both sides or a left/right pair. Records are assigned only
-- through the next width event; remaining vertices carry the latest value.
-- At an event on an existing vertex, the incoming radii are preserved and the
-- new value sets its outgoing radii. Excess records within an event supply
-- incoming/outgoing pairs to its earliest vertices.
-- Curves are flattened within the positive coordinate-space tolerance.
-- Adjacent radii whose difference exceeds the segment length have no common
-- outer tangent and cause an error.
-- Concave joins use a four-radius miter limit and bevel beyond it.
-- @j/J@ join selectors are reported but not interpreted.
pathOutline :: Double -> [Either Supplement PathCommand] -> [PathCommand]
pathOutline tolerance input
  | not (finite tolerance) || tolerance <= 0 =
      error "pathOutline: tolerance must be finite and positive"
  | any (\(left, right) -> not (finite left && finite right) || left < 0 || right < 0) widths =
      error "pathOutline: width radii must be finite and nonnegative"
  | null widthGroups = rights input
  | otherwise =
      let rawSubpaths = decodePath tolerance input
          scopeRadii = assignScopeRadii rawSubpaths widthGroups
          unusedWidths = sum [unused | (_, _, unused) <- scopeRadii]
          sized = attachRadii rawSubpaths scopeRadii
          result = concatMap (uncurry outlineSubpath) sized
          warnings = widthWarnings ++
            [ "pathOutline: unused width arguments in a width event"
            | unusedWidths > 0
            ]
#ifdef DEBUG_OFFSET
          stateLog = unlines
            [ "[SvgTreeOffset] tolerance: " ++ show tolerance
            , "[SvgTreeOffset] width groups: " ++ show widthGroups
            , "[SvgTreeOffset] decoded subpaths: " ++ show rawSubpaths
            , "[SvgTreeOffset] assigned scope radii: " ++ show scopeRadii
            , "[SvgTreeOffset] sized vertices: " ++ show sized
            , "[SvgTreeOffset] warnings: " ++ show warnings
            ]
      in trace stateLog (emitWarnings warnings result)
#else
      in emitWarnings warnings result
#endif
  where
    (widthGroups, widthWarnings) = collectWidths input
    widths = concatMap widthGroupValues widthGroups

emitWarnings :: [String] -> a -> a
emitWarnings warnings result = foldr trace result warnings

finite :: Double -> Bool
finite x = not (isNaN x || isInfinite x)

type WidthPair = (Coord, Coord)
data WidthGroup = WidthGroup
  { widthGroupScope :: !Int
  , widthGroupVertex :: !Int
  , widthGroupHasAnchor :: !Bool
  , widthGroupValues :: ![WidthPair]
  }
#ifdef DEBUG_OFFSET
  deriving (Show)
#endif
data WidthScopeFrame
  = PathScopeFrame !WidthPair !Int !Bool !Int
  | WidthOnlyFrame !WidthPair

data VertexRadii = VertexRadii !Coord !Coord !Coord !Coord
  -- left-in, right-in, left-out, right-out
#ifdef DEBUG_OFFSET
  deriving (Show)
#endif

data Vertex = Vertex !RPoint !VertexRadii
#ifdef DEBUG_OFFSET
  deriving (Show)
#endif

data PathPoint = PathPoint !RPoint ![RPoint]
  -- The extra points are flattened samples between the previous and this point.
#ifdef DEBUG_OFFSET
  deriving (Show)
#endif

data Subpath = Subpath
  { subpathScope :: !Int
  , subpathPoints :: ![PathPoint]
  , subpathClosed :: !Bool
  }
#ifdef DEBUG_OFFSET
  deriving (Show)
#endif


data DecodeState = DecodeState
  { currentPoint :: !RPoint
  , subpathStart :: !(Maybe RPoint)
  , activePointsRev :: ![PathPoint]
  , completedRev :: ![Subpath]
  , activeScope :: !Int
  , previousCubicControl :: !(Maybe RPoint)
  , previousQuadraticControl :: !(Maybe RPoint)
  , previousCurveKind :: !CurveKind
  }
#ifdef DEBUG_OFFSET
  deriving (Show)
#endif

data CurveKind = NoCurve | CubicCurve | QuadraticCurve
  deriving (Eq
#ifdef DEBUG_OFFSET
    , Show
#endif
    )

initialDecodeState :: DecodeState
initialDecodeState = DecodeState
  { currentPoint = V2 0 0
  , subpathStart = Nothing
  , activePointsRev = []
  , completedRev = []
  , activeScope = 0
  , previousCubicControl = Nothing
  , previousQuadraticControl = Nothing
  , previousCurveKind = NoCurve
  }

collectWidths :: [Either Supplement PathCommand] -> ([WidthGroup], [String])
collectWidths input =
  let (groupsRev, _, _, _, warningsRev, _, _, _, _) =
        foldl' step ([], (0, 0), [], False, [], 0, False, 0, 1) input
  in (reverse groupsRev, reverse warningsRev)
  where
    step (groups, current, penStack, pending, warnings, vertexIndex, active, scope, nextScope) command = case command of
      Right pathCommand ->
        let (nextIndex, nextActive) = advancePathCursor vertexIndex active pathCommand
        in (groups, current, penStack, False, warnings, nextIndex, nextActive, scope, nextScope)
      Left (SWidth args) ->
        addArgs False args groups current penStack pending warnings vertexIndex active scope nextScope
      Left (Swidth args) ->
        addArgs True args groups current penStack pending warnings vertexIndex active scope nextScope
      Left (SJoin args) ->
        warnJoin args groups current penStack pending warnings vertexIndex active scope nextScope
      Left (Sjoin args) ->
        warnJoin args groups current penStack pending warnings vertexIndex active scope nextScope
      Left SOpen ->
        (groups, current, PathScopeFrame current vertexIndex active scope : penStack,
          pending, warnings, 1, True, nextScope, nextScope + 1)
      Left SClose -> case penStack of
        PathScopeFrame savedCurrent savedIndex savedActive savedScope : rest ->
          let groups' =
                if vertexIndex > 1 && not (any ((== scope) . widthGroupScope) groups)
                  then appendWidthGroup scope 0 False [savedCurrent] groups
                  else groups
          in (groups', savedCurrent, rest, pending, warnings, savedIndex, savedActive, savedScope, nextScope)
        _ -> (groups, current, penStack, pending, warnings, vertexIndex, active, scope, nextScope)
      Left SOpenW ->
        (groups, current, WidthOnlyFrame current : penStack,
          pending, warnings, vertexIndex, active, scope, nextScope)
      Left SCloseW -> case penStack of
        WidthOnlyFrame savedCurrent : rest ->
          let anchor = active && vertexIndex > 0
              groups' = appendWidthGroup scope vertexIndex anchor [savedCurrent] groups
          in (groups', savedCurrent, rest, pending, warnings,
              vertexIndex, active, scope, nextScope)
        _ -> (groups, current, penStack, pending, warnings, vertexIndex, active, scope, nextScope)

    warnJoin [] groups current penStack pending warnings vertexIndex active scope nextScope =
      (groups, current, penStack, pending, warnings, vertexIndex, active, scope, nextScope)
    warnJoin _ groups current penStack pending warnings vertexIndex active scope nextScope =
      (groups, current, penStack, pending, "pathOutline: j/J join flags are ignored" : warnings,
        vertexIndex, active, scope, nextScope)

    addArgs _ [] groups current penStack pending warnings vertexIndex active scope nextScope =
      (groups, current, penStack, pending, warnings, vertexIndex, active, scope, nextScope)
    addArgs relative args groups current penStack pending warnings vertexIndex active scope nextScope =
      let warning =
            [ "pathOutline: consecutive w/W commands add to a pending width group"
            | pending
            ]
          (resolvedRev, current') = foldl' addOne ([], current) args
          anchor = active && vertexIndex > 0
          groups' = appendWidthGroup scope vertexIndex anchor (reverse resolvedRev) groups
      in (groups', current', penStack, True, reverse warning ++ warnings,
          vertexIndex, active, scope, nextScope)
      where
        addOne (acc, previous) arg =
          let value = widthPair arg
              next = if relative then addPair previous value else value
          in (next : acc, next)

    appendWidthGroup scope index anchor values groups = case groups of
      WidthGroup previousScope previousIndex previousAnchor previousValues : rest
        | previousScope == scope && previousIndex == index && previousAnchor == anchor ->
            WidthGroup scope index anchor (previousValues ++ values) : rest
      _ -> WidthGroup scope index anchor values : groups

widthPair :: WArg -> WidthPair
widthPair (W1 radius) = (radius, radius)
widthPair (W2 left right) = (left, right)

addPair :: WidthPair -> WidthPair -> WidthPair
addPair (left, right) (deltaLeft, deltaRight) =
  (left + deltaLeft, right + deltaRight)
advancePathCursor :: Int -> Bool -> PathCommand -> (Int, Bool)
advancePathCursor vertexIndex active command = case command of
  MoveTo _ points
    | null points -> (vertexIndex, active)
    | otherwise -> (vertexIndex + length points, True)
  EndPath -> (vertexIndex, False)
  _ ->
    let count = pathCommandVertexCount command
    in if count == 0
         then (vertexIndex, active)
         else (vertexIndex + count + if active then 0 else 1, True)

pathCommandVertexCount :: PathCommand -> Int
pathCommandVertexCount command = case command of
  MoveTo _ points -> length points
  LineTo _ points -> length points
  HorizontalTo _ values -> length values
  VerticalTo _ values -> length values
  CurveTo _ curves -> length curves
  SmoothCurveTo _ curves -> length curves
  QuadraticBezier _ curves -> length curves
  SmoothQuadraticBezierCurveTo _ endpoints -> length endpoints
  EllipticalArc _ arcs -> length arcs
  EndPath -> 0

assignWidthGroups :: Int -> [WidthGroup] -> ([VertexRadii], Int)
assignWidthGroups vertexCount groups = applyGroups
  (replicate vertexCount (sameRadii (0, 0))) 0 groups
  where
    applyGroups radii unused [] = (radii, unused)
    applyGroups radii unused (group : later) =
      let index = widthGroupVertex group
          anchored = widthGroupHasAnchor group
          start = max 0 (min vertexCount (if anchored then index - 1 else index))
          end = max start (min vertexCount (case later of
            next : _ -> widthGroupVertex next
            [] -> vertexCount))
          slots = end - start
          values = widthGroupValues group
          (assigned, extra) = assignRadii slots values
          overflow = length values > slots
          (before, suffix) = splitAt start radii
          (oldRange, after) = splitAt slots suffix
          adjusted = case (anchored, overflow, oldRange, assigned) of
            (True, False, oldFirst : _, newFirst : rest) ->
              preserveIncoming oldFirst newFirst : rest
            _ -> assigned
      in applyGroups (before ++ adjusted ++ after) (unused + extra) later

    preserveIncoming (VertexRadii leftIn rightIn _ _)
      (VertexRadii _ _ leftOut rightOut) =
        VertexRadii leftIn rightIn leftOut rightOut

assignRadii :: Int -> [WidthPair] -> ([VertexRadii], Int)
assignRadii vertexCount widths = go vertexCount (length widths) widths (0, 0)
  where
    go 0 remaining _ _ = ([], remaining)
    go verticesLeft 0 _ previous =
      (replicate verticesLeft (sameRadii previous), 0)
    go verticesLeft widthCount (incoming : outgoing : rest) previous
      | widthCount > verticesLeft =
          let (later, unused) = go (verticesLeft - 1) (widthCount - 2) rest outgoing
          in (VertexRadii (fst incoming) (snd incoming) (fst outgoing) (snd outgoing) : later, unused)
    go verticesLeft widthCount (width : rest) _ =
      let (later, unused) = go (verticesLeft - 1) (widthCount - 1) rest width
      in (sameRadii width : later, unused)

sameRadii :: WidthPair -> VertexRadii
sameRadii (left, right) = VertexRadii left right left right

assignScopeRadii :: [Subpath] -> [WidthGroup] -> [(Int, [VertexRadii], Int)]
assignScopeRadii subpaths groups = foldl' assign [] scopes
  where
    scopes = map subpathScope subpaths ++ map widthGroupScope groups
    assign assigned scope
      | any ((== scope) . firstOfThree) assigned = assigned
      | otherwise =
          let vertexCount = sum
                [ length points
                | Subpath pathScope points _ <- subpaths
                , pathScope == scope
                ]
              (radii, unused) = assignWidthGroups vertexCount
                (filter ((== scope) . widthGroupScope) groups)
          in (scope, radii, unused) : assigned
    firstOfThree (scope, _, _) = scope

attachRadii :: [Subpath] -> [(Int, [VertexRadii], Int)] -> [([Vertex], Bool)]
attachRadii subpaths scopedRadii = go [] subpaths
  where
    go _ [] = []
    go offsets (Subpath scope points closed : rest) =
      let radii = case [values | (candidate, values, _) <- scopedRadii, candidate == scope] of
            values : _ -> values
            [] -> []
          offset = maybe 0 id (lookup scope offsets)
          (_, afterOffset) = splitAt offset radii
          (here, _) = splitAt (length points) afterOffset
          nextOffsets = (scope, offset + length points) : filter ((/= scope) . fst) offsets
      in (sizePoints points here, closed) : go nextOffsets rest

sizePoints :: [PathPoint] -> [VertexRadii] -> [Vertex]
sizePoints [] _ = []
sizePoints (PathPoint first _ : rest) (firstRadii : radii) =
  Vertex first firstRadii : go (Vertex first firstRadii) rest radii
  where
    go _ [] _ = []
    go previous (PathPoint point samples : points) (currentRadii : remaining) =
      let samples' = sampleRadii previous currentRadii samples
      in samples' ++ Vertex point currentRadii : go (Vertex point currentRadii) points remaining
    go _ _ [] = []
sizePoints _ _ = []

sampleRadii :: Vertex -> VertexRadii -> [RPoint] -> [Vertex]
sampleRadii _ _ [] = []
sampleRadii (Vertex _ (VertexRadii _ _ leftOut rightOut))
  (VertexRadii leftIn rightIn _ _) samples =
    let count = length samples + 1
        at i = fromIntegral i / fromIntegral count
        lerp a b t = a + (b - a) * t
        sample (index, point) =
          let t = at index
              left = lerp leftOut leftIn t
              right = lerp rightOut rightIn t
          in Vertex point (VertexRadii left right left right)
    in map sample (zip [1 ..] samples)

outlineSubpath :: [Vertex] -> Bool -> [PathCommand]
outlineSubpath vertices closed = case collapseVertices vertices closed of
  [] -> []
  [Vertex point radii] -> pointOutline point radii
  normalized
    | closed ->
        sidePath LeftSide 1 True normalized ++ [EndPath]
          ++ sidePath RightSide 1 True (reverse (map reverseRadii normalized)) ++ [EndPath]
    | otherwise ->
        case (sidePath LeftSide 1 False normalized,
              sidePath RightSide 1 False (reverse (map reverseRadii normalized))) of
          ((MoveTo _ (leftStart : _) : leftRest),
           (MoveTo _ (rightEnd : _) : rightRest)) ->
             MoveTo OriginAbsolute [leftStart]
               : leftRest ++ [LineTo OriginAbsolute [rightEnd]] ++ rightRest ++ [EndPath]
          (left, right) -> left ++ right

collapseVertices :: [Vertex] -> Bool -> [Vertex]
collapseVertices vertices closed =
  let collapsed = reverse (foldl' push [] vertices)
  in if closed then mergeClosedSeam collapsed else collapsed
  where
    push [] vertex = [vertex]
    push (previous : rest) vertex
      | vertexPoint previous == vertexPoint vertex = mergeAdjacent previous vertex : rest
      | otherwise = vertex : previous : rest

    mergeAdjacent (Vertex point (VertexRadii leftIn rightIn _ _))
      (Vertex _ (VertexRadii _ _ leftOut rightOut)) =
        Vertex point (VertexRadii leftIn rightIn leftOut rightOut)

    mergeClosedSeam [] = []
    mergeClosedSeam [vertex] = [vertex]
    mergeClosedSeam points@(first : _) = case reverse points of
      lastVertex : _
        | vertexPoint first == vertexPoint lastVertex ->
            mergeSeam first lastVertex : init (tail points)
      _ -> points

    mergeSeam (Vertex point (VertexRadii _ _ leftOut rightOut))
      (Vertex _ (VertexRadii leftIn rightIn _ _)) =
        Vertex point (VertexRadii leftIn rightIn leftOut rightOut)
pointOutline :: RPoint -> VertexRadii -> [PathCommand]
pointOutline point (VertexRadii leftIn rightIn leftOut rightOut) =
  let radius = maximum [leftIn, rightIn, leftOut, rightOut]
      start = point + V2 radius 0
      opposite = point - V2 radius 0
  in if radius == 0
       then [MoveTo OriginAbsolute [point], EndPath]
       else
         [ MoveTo OriginAbsolute [start]
         , EllipticalArc OriginAbsolute
             [ (radius, radius, 0, False, True, opposite)
             , (radius, radius, 0, False, True, start)
             ]
         , EndPath
         ]

data Side = LeftSide | RightSide


incomingRadius :: Side -> Vertex -> Coord
incomingRadius LeftSide (Vertex _ (VertexRadii left _ _ _)) = left
incomingRadius RightSide (Vertex _ (VertexRadii _ right _ _)) = right

outgoingRadius :: Side -> Vertex -> Coord
outgoingRadius LeftSide (Vertex _ (VertexRadii _ _ left _)) = left
outgoingRadius RightSide (Vertex _ (VertexRadii _ _ _ right)) = right

reverseRadii :: Vertex -> Vertex
reverseRadii (Vertex point (VertexRadii leftIn rightIn leftOut rightOut)) =
  Vertex point (VertexRadii leftOut rightOut leftIn rightIn)


sidePath :: Side -> Coord -> Bool -> [Vertex] -> [PathCommand]
sidePath _ _ _ [] = []
sidePath _ _ _ [_] = []
sidePath side normalSign closed vertices@(first : second : _) =
  let edges = zip vertices (if closed then tail vertices ++ take 1 vertices else tail vertices)
      (firstStart, _, _) = segmentTangent side normalSign first second
      adjacent = if closed
        then zip edges (map Just (tail edges ++ take 1 edges))
        else zip edges (map Just (tail edges) ++ [Nothing])
      body = concatMap (edgeCommands side normalSign) adjacent
  in MoveTo OriginAbsolute [firstStart] : body

edgeCommands
  :: Side
  -> Coord
  -> ((Vertex, Vertex), Maybe (Vertex, Vertex))
  -> [PathCommand]
edgeCommands side normalSign ((start, end), nextEdge) =
  let direction = vertexPoint end - vertexPoint start
      (startPoint, endPoint, incomingNormal) = segmentTangent side normalSign start end
  in case nextEdge of
       Nothing -> [LineTo OriginAbsolute [endPoint]]
       Just (nextStart, nextEnd) ->
         let nextDirection = vertexPoint nextEnd - vertexPoint nextStart
             (_, _, outgoingNormal) = segmentTangent side normalSign nextStart nextEnd
             (joinPoint, joinCommands) =
               joinAt side end direction nextDirection incomingNormal outgoingNormal
         -- The miter replaces the tangent endpoint; do not retrace it.
         in lineTo startPoint joinPoint ++ joinCommands

segmentTangent :: Side -> Coord -> Vertex -> Vertex -> (RPoint, RPoint, RPoint)
segmentTangent side normalSign start end
  | distance == 0 || not (finite distance) =
      error "pathOutline: adjacent path vertices must have distinct finite coordinates"
  | not (finite ratio) || abs ratio > 1 + 1e-12 =
      error "pathOutline: adjacent radii do not admit a common outer tangent"
  | otherwise = (startPoint, endPoint, tangentNormal)
  where
    direction = vertexPoint end - vertexPoint start
    distance = magnitude direction
    startRadius = outgoingRadius side start
    endRadius = incomingRadius side end
    ratio = (startRadius - endRadius) / distance
    boundedRatio = max (-1) (min 1 ratio)
    tangentNormal = direction ^/ distance ^* boundedRatio
      + unitNormal direction ^* (normalSign * sqrt (max 0 (1 - boundedRatio * boundedRatio)))
    startPoint = vertexPoint start + tangentNormal ^* startRadius
    endPoint = vertexPoint end + tangentNormal ^* endRadius

joinAt :: Side -> Vertex -> RPoint -> RPoint -> RPoint -> RPoint -> (RPoint, [PathCommand])
joinAt side vertex incomingDirection outgoingDirection incomingNormal outgoingNormal
  | abs turn < 1e-12 = (incomingPoint, lineTo incomingPoint outgoingPoint)
  | abs turn >= pi - 1e-12 = (incomingPoint, lineTo incomingPoint outgoingPoint)
  | turn > 0 = outerJoin
  | otherwise = innerJoin
  where
    point = vertexPoint vertex
    incoming = incomingRadius side vertex
    outgoing = outgoingRadius side vertex
    incomingPoint = point + incomingNormal ^* incoming
    outgoingPoint = point + outgoingNormal ^* outgoing
    turn = atan2 (cross incomingDirection outgoingDirection)
                  (dot incomingDirection outgoingDirection)
    -- Slide the arc center along the incoming tangent to meet the outgoing offset.
    outerJoin =
      let incomingTangent = incomingDirection ^/ magnitude incomingDirection
          normalProjection = dot incomingTangent outgoingNormal
          centerShift = incomingTangent ^* ((outgoing - incoming) / normalProjection)
          arcCenter = point + centerShift
          arcStart = arcCenter + incomingNormal ^* incoming
          arcEnd = arcCenter + outgoingNormal ^* incoming
          arc = if incoming == 0 then [] else
            [EllipticalArc OriginAbsolute
              [(incoming, incoming, 0, False, True, arcEnd)]]
          miterLimit = 4 * max incoming outgoing
      in if abs normalProjection < 1e-12 || not (finitePoint arcCenter)
             || magnitude centerShift > miterLimit
           then (incomingPoint, lineTo incomingPoint outgoingPoint)
           else (arcStart, arc ++ lineTo arcEnd outgoingPoint)
    innerJoin =
      let incomingTangent = perpendicular incomingNormal
          outgoingTangent = perpendicular outgoingNormal
          determinant = cross incomingTangent outgoingTangent
          intersection =
            incomingPoint + incomingTangent ^*
              (cross (outgoingPoint - incomingPoint) outgoingTangent / determinant)
          miterLimit = 4 * max incoming outgoing
      in if abs determinant < 1e-12 || not (finitePoint intersection)
             || magnitude (intersection - point) > miterLimit
           then (incomingPoint, lineTo incomingPoint outgoingPoint)
           else (intersection, [])

perpendicular :: RPoint -> RPoint
perpendicular (V2 x y) = V2 (-y) x

finitePoint :: RPoint -> Bool
finitePoint (V2 x y) = finite x && finite y

vertexPoint :: Vertex -> RPoint
vertexPoint (Vertex point _) = point

lineTo :: RPoint -> RPoint -> [PathCommand]
lineTo start end
  | start == end = []
  | otherwise = [LineTo OriginAbsolute [end]]

unitNormal :: RPoint -> RPoint
unitNormal (V2 x y)
  | magnitude == 0 = V2 0 0
  | otherwise = V2 (y / magnitude) (-x / magnitude)
  where
    magnitude = sqrt (x * x + y * y)

decodePath :: Double -> [Either Supplement PathCommand] -> [Subpath]
decodePath tolerance commands =
  let (finalState, stack, _) = foldl' step (initialDecodeState, [], 1) commands
      finished = finishScopes finalState stack
#ifdef DEBUG_OFFSET
      subpaths = reverse (completedRev finished)
  in trace
      (unlines
        [ "[SvgTreeOffset] final decode state: " ++ show finalState
        , "[SvgTreeOffset] saved decode states: " ++ show stack
        , "[SvgTreeOffset] decoded subpaths: " ++ show subpaths
        ])
      subpaths
#else
  in reverse (completedRev finished)
#endif
  where
    step (state, stack, nextScope) command = case command of
      Right pathCommand -> (decodeCommand tolerance state pathCommand, stack, nextScope)
      Left SOpen ->
        let branchState = clearControls (state
              { subpathStart = Nothing
              , activePointsRev = []
              , activeScope = nextScope
              })
        in (branchState, state : stack, nextScope + 1)
      Left SClose -> case stack of
        saved : rest ->
          let branchFinished = finishActive False state
          in (saved { completedRev = completedRev branchFinished }, rest, nextScope)
        [] -> (state, stack, nextScope)
      Left _ -> (state, stack, nextScope)

    finishScopes state [] = finishActive False state
    finishScopes state (saved : rest) =
      let branchFinished = finishActive False state
      in finishScopes (saved { completedRev = completedRev branchFinished }) rest


decodeCommand :: Double -> DecodeState -> PathCommand -> DecodeState
decodeCommand tolerance state command = case command of
  MoveTo origin points -> case points of
    [] -> clearControls state
    first : remaining ->
      let start = resolvePoint origin (currentPoint state) first
          begun = beginSubpath start state
      in foldl' (appendPoint origin) begun remaining
  LineTo origin points -> foldl' (appendPoint origin) state points
  HorizontalTo origin values -> foldl' appendHorizontal state values
    where
      appendHorizontal s value =
        let V2 x y = currentPoint s
            nextX = if origin == OriginAbsolute then value else x + value
        in appendLine (V2 nextX y) s
  VerticalTo origin values -> foldl' appendVertical state values
    where
      appendVertical s value =
        let V2 x y = currentPoint s
            nextY = if origin == OriginAbsolute then value else y + value
        in appendLine (V2 x nextY) s
  CurveTo origin curves -> foldl' appendCubic state curves
    where
      appendCubic s (control1, control2, endpoint) =
        let start = currentPoint s
            c1 = resolvePoint origin start control1
            c2 = resolvePoint origin start control2
            end = resolvePoint origin start endpoint
            samples = flattenCubic tolerance start c1 c2 end
        in setCubicControl c2 (appendSegment end samples s)
  SmoothCurveTo origin curves -> foldl' appendSmoothCubic state curves
    where
      appendSmoothCubic s (control2, endpoint) =
        let start = currentPoint s
            control1 = case (previousCurveKind s, previousCubicControl s) of
              (CubicCurve, Just previous) -> 2 *^ start - previous
              _ -> start
            c2 = resolvePoint origin start control2
            end = resolvePoint origin start endpoint
            samples = flattenCubic tolerance start control1 c2 end
        in setCubicControl c2 (appendSegment end samples s)
  QuadraticBezier origin curves -> foldl' appendQuadratic state curves
    where
      appendQuadratic s (control, endpoint) =
        let start = currentPoint s
            c = resolvePoint origin start control
            end = resolvePoint origin start endpoint
            samples = flattenQuadratic tolerance start c end
        in setQuadraticControl c (appendSegment end samples s)
  SmoothQuadraticBezierCurveTo origin endpoints ->
    foldl' appendSmoothQuadratic state endpoints
    where
      appendSmoothQuadratic s endpoint =
        let start = currentPoint s
            control = case (previousCurveKind s, previousQuadraticControl s) of
              (QuadraticCurve, Just previous) -> 2 *^ start - previous
              _ -> start
            end = resolvePoint origin start endpoint
            samples = flattenQuadratic tolerance start control end
        in setQuadraticControl control (appendSegment end samples s)
  EllipticalArc origin arcs -> foldl' appendArc state arcs
    where
      appendArc s (rx, ry, rotation, largeArc, sweep, endpoint) =
        let start = currentPoint s
            end = resolvePoint origin start endpoint
            samples = flattenArc tolerance start rx ry rotation largeArc sweep end
        in clearControls (appendSegment end samples s)
  EndPath -> closeSubpath state

appendPoint :: Origin -> DecodeState -> RPoint -> DecodeState
appendPoint origin state point =
  appendLine (resolvePoint origin (currentPoint state) point) state

appendLine :: RPoint -> DecodeState -> DecodeState
appendLine point = clearControls . appendSegment point []

appendSegment :: RPoint -> [RPoint] -> DecodeState -> DecodeState
appendSegment endpoint samples state =
  let active = ensureActive state
  in active
       { currentPoint = endpoint
       , activePointsRev = PathPoint endpoint samples : activePointsRev active
       }

beginSubpath point state =
  let finished = finishActive False state
  in clearControls (finished
       { currentPoint = point
       , subpathStart = Just point
       , activePointsRev = [PathPoint point []]
       })

ensureActive :: DecodeState -> DecodeState
ensureActive state
  | not (null (activePointsRev state)) = state
  | otherwise = state
      { subpathStart = Just (currentPoint state)
      , activePointsRev = [PathPoint (currentPoint state) []]
      }

finishActive :: Bool -> DecodeState -> DecodeState
finishActive closed state
  | null (activePointsRev state) = state { subpathStart = Nothing }
  | otherwise = state
      { subpathStart = Nothing
      , activePointsRev = []
      , completedRev = Subpath (activeScope state) (reverse (activePointsRev state)) closed : completedRev state
      }

closeSubpath :: DecodeState -> DecodeState
closeSubpath state =
  let start = subpathStart state
      finished = finishActive True state
      current = maybe (currentPoint state) id start
  in clearControls (finished { currentPoint = current })

clearControls :: DecodeState -> DecodeState
clearControls state = state
  { previousCubicControl = Nothing
  , previousQuadraticControl = Nothing
  , previousCurveKind = NoCurve
  }

setCubicControl :: RPoint -> DecodeState -> DecodeState
setCubicControl control state = state
  { previousCubicControl = Just control
  , previousQuadraticControl = Nothing
  , previousCurveKind = CubicCurve
  }

setQuadraticControl :: RPoint -> DecodeState -> DecodeState
setQuadraticControl control state = state
  { previousCubicControl = Nothing
  , previousQuadraticControl = Just control
  , previousCurveKind = QuadraticCurve
  }

resolvePoint :: Origin -> RPoint -> RPoint -> RPoint
resolvePoint OriginAbsolute _ point = point
resolvePoint OriginRelative current point = current + point

flattenCubic :: Double -> RPoint -> RPoint -> RPoint -> RPoint -> [RPoint]
flattenCubic tolerance p0 p1 p2 p3 = go 0 p0 p1 p2 p3
  where
    go depth a b c d
      | max (distanceToSegment b a d) (distanceToSegment c a d) <= tolerance = []
      | depth >= 32 = error "pathOutline: cubic flattening exceeded recursion limit"
      | otherwise =
          let ab = midpoint a b
              bc = midpoint b c
              cd = midpoint c d
              abc = midpoint ab bc
              bcd = midpoint bc cd
              middle = midpoint abc bcd
          in go (depth + 1) a ab abc middle
               ++ [middle]
               ++ go (depth + 1) middle bcd cd d

flattenQuadratic :: Double -> RPoint -> RPoint -> RPoint -> [RPoint]
flattenQuadratic tolerance p0 p1 p2 = go 0 p0 p1 p2
  where
    go depth a b c
      | distanceToSegment b a c <= tolerance = []
      | depth >= 32 = error "pathOutline: quadratic flattening exceeded recursion limit"
      | otherwise =
          let ab = midpoint a b
              bc = midpoint b c
              middle = midpoint ab bc
          in go (depth + 1) a ab middle
               ++ [middle]
               ++ go (depth + 1) middle bc c

flattenArc
  :: Double -> RPoint -> Coord -> Coord -> Coord -> Bool -> Bool -> RPoint -> [RPoint]
flattenArc tolerance start rx0 ry0 rotation largeArc sweep end
  | start == end || rx0 == 0 || ry0 == 0 = []
  | otherwise =
      let phi = rotation * pi / 180
          cosPhi = cos phi
          sinPhi = sin phi
          V2 x1 y1 = start
          V2 x2 y2 = end
          dx = (x1 - x2) / 2
          dy = (y1 - y2) / 2
          xp = cosPhi * dx + sinPhi * dy
          yp = -sinPhi * dx + cosPhi * dy
          (rx, ry) = correctedRadii (abs rx0) (abs ry0) xp yp
          rx2 = rx * rx
          ry2 = ry * ry
          denominator = rx2 * yp * yp + ry2 * xp * xp
          numerator = max 0 (rx2 * ry2 - rx2 * yp * yp - ry2 * xp * xp)
          signum' = if largeArc == sweep then -1 else 1
          factor = signum' * sqrt (numerator / denominator)
          cxp = factor * rx * yp / ry
          cyp = factor * negate (ry * xp / rx)
          V2 cx cy = V2
            (cosPhi * cxp - sinPhi * cyp + (x1 + x2) / 2)
            (sinPhi * cxp + cosPhi * cyp + (y1 + y2) / 2)
          ux = (xp - cxp) / rx
          uy = (yp - cyp) / ry
          vx = (-xp - cxp) / rx
          vy = (-yp - cyp) / ry
          theta = atan2 uy ux
          rawDelta = atan2 (ux * vy - uy * vx) (ux * vx + uy * vy)
          delta
            | sweep && rawDelta < 0 = rawDelta + 2 * pi
            | not sweep && rawDelta > 0 = rawDelta - 2 * pi
            | otherwise = rawDelta
          maxRadius = max rx ry
          maxStep
            | tolerance >= 2 * maxRadius = 2 * pi
            | otherwise = 2 * acos (max (-1) (min 1 (1 - tolerance / maxRadius)))
          requiredSegments = abs delta / maxStep
          pointAt count i =
            let angle = theta + delta * fromIntegral i / fromIntegral count
                ex = rx * cos angle
                ey = ry * sin angle
            in V2 (cx + cosPhi * ex - sinPhi * ey)
                  (cy + sinPhi * ex + cosPhi * ey)
      in if not (finite denominator) || denominator == 0
           then []
           else if not (finite maxStep) || maxStep <= 0
                   || not (finite requiredSegments) || requiredSegments > 1000000
             then error "pathOutline: arc flattening requires over one million segments"
             else let count = max 1 (ceiling requiredSegments :: Int)
                  in map (pointAt count) [1 .. count - 1]

correctedRadii :: Coord -> Coord -> Coord -> Coord -> (Coord, Coord)
correctedRadii rx ry xp yp
  | lambda <= 1 = (rx, ry)
  | otherwise = (rx * scale, ry * scale)
  where
    lambda = xp * xp / (rx * rx) + yp * yp / (ry * ry)
    scale = sqrt lambda

distanceToSegment :: RPoint -> RPoint -> RPoint -> Double
distanceToSegment point start end
  | squaredLength == 0 = magnitude (point - start)
  | otherwise = magnitude (point - (start + direction ^* amount))
  where
    direction = end - start
    squaredLength = dot direction direction
    amount = max 0 (min 1 (dot (point - start) direction / squaredLength))

midpoint :: RPoint -> RPoint -> RPoint
midpoint a b = (a + b) ^/ 2

magnitude :: RPoint -> Double
magnitude (V2 x y) = sqrt (x * x + y * y)

dot :: RPoint -> RPoint -> Double
dot (V2 ax ay) (V2 bx by) = ax * bx + ay * by

cross :: RPoint -> RPoint -> Double
cross (V2 ax ay) (V2 bx by) = ax * by - ay * bx

