module SvgTreeAntiquote (parsePathCommands, parsePathCommandsSupplement, Supplement(..), WArg(..), JArg(..)) where

import Data.Functor ((<&>))
import Graphics.Svg.Types (Origin (..), PathCommand (..))
import Language.Haskell.TH
  ( Exp,
    ExpQ,
    Q,
    appsE,
    dyn,
    listE,
    reportError,
    tupE,
  )
import Language.LBNF
  ( AlexInput,
    Array,
    BNFC_QQType,
    HappyStk (..),
    ParseMonad (Bad, Ok),
    Posn (..),
    alexGetByte,
    appEPAll,
    appEPAllL,
    bnfc,
    fromLit,
    fromToken,
    lbnf,
    listArray,
    (!),
  )
import Language.LBNF.Runtime (err)
import Linear (V2 (V2))
import Prelude hiding (exp)
import Language.Haskell.TH.Syntax ( lookupValueName, Lift(lift) )
import Data.Maybe ( catMaybes, fromMaybe )
import Data.List ( groupBy, stripPrefix )
import Data.Either (fromLeft)

bnfc
  [lbnf|

entrypoints Cmd, Cmds;

comment "--";
separator Cmd "";
Cmds.Cmds ::= [Cmd];

Ca.Cmd ::= "a" [AArg];
CA.Cmd ::= "A" [AArg];
Ct.Cmd ::= "t" [MArg];
CT.Cmd ::= "T" [MArg];
Cl.Cmd ::= "l" [MArg];
CL.Cmd ::= "L" [MArg];
CQ.Cmd ::= "Q" [QArg];
Cq.Cmd ::= "q" [QArg];
CS.Cmd ::= "S" [QArg];
Cs.Cmd ::= "s" [QArg];
CC.Cmd ::= "C" [CArg];
Cc.Cmd ::= "c" [CArg];
Ch.Cmd ::= "h" [HArg];
CH.Cmd ::= "H" [HArg];
Cv.Cmd ::= "v" [HArg];
CV.Cmd ::= "V" [HArg];
Cm.Cmd ::= "m" [MArg];
CM.Cmd ::= "M" [MArg];
Cw.Cmd ::= "w" [WArgE];
CW.Cmd ::= "W" [WArgE];
Cj.Cmd ::= "j" [JArg];
CJ.Cmd ::= "J" [JArg];
CZ.Cmd ::= "z";
CZ.Cmd ::= "Z";
COpen.Cmd ::= "{";
CClose.Cmd ::= "}";
COpenW.Cmd ::= "[";
CCloseW.Cmd ::= "]";

W1E.WArgE ::= E;
W2E.WArgE ::= E "," E;
J1.JArg ::= Integer;
J2.JArg ::= Integer "," Integer;
AArg.AArg::= E "," E "," E "," B "," B "," MArg;
QArg.QArg ::= MArg "," MArg;
CArg.CArg ::= MArg "," MArg"," MArg;
MArg.MArg::= E "," E;
HArg.HArg ::= E;
separator AArg "";
separator MArg "";
separator QArg "";
separator CArg "";
separator HArg "";
separator JArg "";
separator WArgE "";


EDouble.E4 ::= Double;
EInt. E4 ::= Integer;
EVar. E4 ::= Ident [E1];
EOp . E3 ::= E4 Op3 E3;
EOp . E2  ::= E2 Op2 E3;
EOp . E1  ::= E1 Op E2;
ENeg. E  ::= "-" E1;

BV.B ::= Ident;
BI.B ::= Integer;

separator E1 "";

EPlus. Op ::= "+";
EMinus. Op ::= "-";
ETimes. Op2 ::= "*";
EDiv. Op2 ::= "/";
EPow. Op3 ::= "**";
EPow. Op3 ::= "^";
EPow. Op3 ::= "^^";


_. E4 ::= "(" E ")" ;
_. E3 ::= E4 ;
_. E2 ::= E3 ;
_. E1 ::= E2 ;
_. E  ::= E1 ;

 |]

-- | Parse svg path commands into svg-tree's 'PathCommand',
-- supporting arithmetic @+ - * / ** ( )@ with haskell-like fixity
--
-- @
-- ghci> let x = 7; y = 3 in $(parsePathCommands "v -x**2 h y")
-- [VerticalTo OriginRelative [-49.0],HorizontalTo OriginRelative [3.0]]
-- @
--
-- Single-letter variables (@atlqschvmz@) are supported: when in-scope
-- ('lookupValueName' won't find declarations after) tokens after the first are
-- mangled and unmangled for the lbnfc parser. In the following example the
-- absolute @T@-command can be rewritten as a relative @t@-command, without
-- interfering with the @t@-variables
--
-- >T t,t
-- >T 0,0
-- >T
-- > t,t
--
-- @
-- ghci> let t = 2 in $(parsePathCommands "t t,t\nt 0,0\nt\n t,t")
-- >[SmoothQuadraticBezierCurveTo OriginRelative [V2 2.0 2.0]
-- >,SmoothQuadraticBezierCurveTo OriginRelative [V2 0.0 0.0]
-- >,SmoothQuadraticBezierCurveTo OriginRelative [V2 2.0 2.0]]
-- @
parsePathCommands :: String -- ^ https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/d#path_commands
  -> ExpQ -- ^ ['PathCommand']
parsePathCommands input = do
  (mangle,unmangle) <- mangleUnmangle
  let ?unmangle = unmangle
  case pCmds $ applyMangle mangle $ myLexer input of
    Ok (Cmds s) -> listE (map (either id (error "unexpected") . toPC) s)
    Bad msg -> reportError msg >> [|()|]

parsePathCommandsSupplement :: String -> ExpQ -- ^ [Either Supplement PathCommand]
parsePathCommandsSupplement input = do
  (mangle,unmangle) <- mangleUnmangle
  let ?unmangle = unmangle
  case pCmds $ applyMangle mangle $ myLexer input of
    Ok (Cmds s) -> listE (map (liftEither . toPC) s)
    Bad msg -> reportError msg >> [|()|]

liftEither :: Either ExpQ ExpQ -> ExpQ
liftEither (Left x) = [| Left $x |]
liftEither (Right x) = [| Right $x |]

-- | hanging indent
applyMangle :: (Token -> Token) -> [Token] -> [Token]
applyMangle f ts = concatMap (mapTail f) $ groupBy sameLineOrIndented ts

sameLineOrIndented (PT (Pn _ x i) _) (PT (Pn _ y j) _) = x == y || j > i
sameLineOrIndented _ _ = False

mapTail f (x:xs) = x : map f xs
mapTail _ [] = []

-- | add a (hopefully) unique prefix
mangleUnmangle :: Q (Token -> Token, String -> String)
mangleUnmangle = do
  let prefix = "oOoOoO"
  ns <- catMaybes <$> sequence [ lookupValueName n <&> fmap (const n)
   | n <- ["a","t","l","q","s","c","h","v","m","z","w","j"]]
  return (\case
    PT p (TS n@[_] _) | n `elem` ns -> PT p (TV (prefix ++ n))
    x -> x
   , \mangled -> fromMaybe mangled $ stripPrefix prefix mangled)

toPC = \case
  Ca args -> Right [|EllipticalArc OriginRelative $(toAArgs args)|]
  CA args -> Right [|EllipticalArc OriginAbsolute $(toAArgs args)|]
  Ct args -> Right [|SmoothQuadraticBezierCurveTo OriginRelative $(toMArgs args)|]
  CT args -> Right [|SmoothQuadraticBezierCurveTo OriginAbsolute $(toMArgs args)|]
  Cl args -> Right [|LineTo OriginRelative $(toMArgs args)|]
  CL args -> Right [|LineTo OriginAbsolute $(toMArgs args)|]
  Cq args -> Right [|QuadraticBezier OriginRelative $(toQArgs args)|]
  CQ args -> Right [|QuadraticBezier OriginAbsolute $(toQArgs args)|]
  Cs args -> Right [|SmoothCurveTo OriginRelative $(toQArgs args)|]
  CS args -> Right [|SmoothCurveTo OriginAbsolute $(toQArgs args)|]
  Cc args -> Right [|CurveTo OriginRelative $(toCArgs args)|]
  CC args -> Right [|CurveTo OriginAbsolute $(toCArgs args)|]
  Ch args -> Right [|HorizontalTo OriginRelative $(toHArgs args)|]
  CH args -> Right [|HorizontalTo OriginAbsolute $(toHArgs args)|]
  Cv args -> Right [|VerticalTo OriginRelative $(toHArgs args)|]
  CV args -> Right [|VerticalTo OriginAbsolute $(toHArgs args)|]
  Cm args -> Right [|MoveTo OriginRelative $(toMArgs args)|]
  CM args -> Right [|MoveTo OriginAbsolute $(toMArgs args)|]
  CZ -> Right [|EndPath|]
  Cw args -> Left [| Swidth $(toWArgs args) |]
  CW args -> Left [| SWidth $(toWArgs args) |]
  Cj args -> Left [| SJoin $(toJArgs args) |]
  COpen -> Left [| SOpen |]
  CClose -> Left [| SClose |]
  COpenW -> Left [| SOpenW |]
  CCloseW -> Left [| SCloseW |]

-- | for svg-tree-offset:SvgTreeOffset
data Supplement = SWidth [WArg] | Swidth [WArg] | SJoin [JArg] | Sjoin [JArg] | SOpen | SClose | SOpenW | SCloseW

data WArg = W1 Double | W2 Double Double

deriving instance Lift JArg

toWArgs = listE . map toWArg

toWArg = \case
  W1E e -> [| W1 $(toExp e) |]
  W2E e f -> [| W2 $(toExp e) $(toExp f) |]

toJArgs = listE . map lift

toHArgs = listE . map toCoord

toMArgs = listE . map toV2

toV2 (MArg x y) = [|V2 $(toExp x) $(toExp y)|]

toCoord (HArg x) = toExp x

toExp = \case
  EDouble d -> [|d|]
  EInt i -> [|fromIntegral i|]
  EVar (Ident a) b -> appsE (dyn (?unmangle a) : map toExp b)
  ENeg e -> [| - $(toExp e) |]
  EOp a op b -> liftOp op (toExp a) (toExp b)

liftOp :: Op -> ExpQ -> ExpQ -> ExpQ
liftOp op a b = case op of
  EPlus -> [| $a + $b |]
  EMinus -> [| $a - $b |]
  ETimes -> [| $a * $b |]
  EDiv -> [| $a / $b |]
  EPow -> [| $a ** $b |]

toQArgs = listE . map toQArg

toQArg (QArg a b) = tupE [toV2 a, toV2 b]

toCArgs = listE . map toCArg

toCArg (CArg a b c) = tupE [toV2 a, toV2 b, toV2 c]

toAArg (AArg rx ry rot large sweep xy) =
  tupE
    [ toExp rx,
      toExp ry,
      toExp rot,
      toExpB large,
      toExpB sweep,
      toV2 xy
    ]

toExpB = \case
  BV (Ident b) -> dyn (?unmangle b)
  BI n
    | n == 1 -> [| True |]
    | n == 0 -> [| False |]

toAArgs = listE . map toAArg
