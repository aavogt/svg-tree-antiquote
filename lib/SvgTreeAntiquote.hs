module SvgTreeAntiquote (parsePathCommands) where

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
CZ.Cmd ::= "z";
CZ.Cmd ::= "Z";

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


EDouble.E3 ::= Double;
EInt. E3 ::= Integer;
EVar. E3 ::= Ident [E];
EOp . E2 ::= E3 Op3 E2;
EOp . E1  ::= E1 Op2 E2;
EOp . E  ::= E Op E1;
ENeg. E  ::= "-" E;

BV.B ::= Ident;
BI.B ::= Integer;

separator E "";

EPlus. Op ::= "+";
EMinus. Op ::= "-";
ETimes. Op2 ::= "*";
EDiv. Op2 ::= "/";
EPow. Op3 ::= "**";
EPow. Op3 ::= "^";
EPow. Op3 ::= "^^";


_. E3 ::= "(" E ")" ;
_. E2 ::= E3 ;
_. E1 ::= E2 ;
_. E  ::= E1 ;

 |]

-- | Parse svg path commands into svg-tree's 'PathCommand',
-- supporting arithmetic @+ - * / ** ( )@ with haskell-like fixity
--
-- ghci> let x = 7; y = 3 in $(parsePathCommands "v -x**2 h y")
-- [VerticalTo OriginRelative [-49.0],HorizontalTo OriginRelative [3.0]]
parsePathCommands :: String -- ^ https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/d#path_commands
  -> ExpQ -- ^ ['PathCommand']
parsePathCommands input = case pCmds (myLexer input) of
  Ok (Cmds s) -> listE (map toPC s)
  Bad msg -> reportError msg >> [|()|]

toPC :: Cmd -> ExpQ
toPC = \case
  Ca args -> [|EllipticalArc OriginRelative $(toAArgs args)|]
  CA args -> [|EllipticalArc OriginAbsolute $(toAArgs args)|]
  Ct args -> [|SmoothQuadraticBezierCurveTo OriginRelative $(toMArgs args)|]
  CT args -> [|SmoothQuadraticBezierCurveTo OriginAbsolute $(toMArgs args)|]
  Cl args -> [|LineTo OriginRelative $(toMArgs args)|]
  CL args -> [|LineTo OriginAbsolute $(toMArgs args)|]
  Cq args -> [|QuadraticBezier OriginRelative $(toQArgs args)|]
  CQ args -> [|QuadraticBezier OriginAbsolute $(toQArgs args)|]
  Cs args -> [|SmoothCurveTo OriginRelative $(toQArgs args)|]
  CS args -> [|SmoothCurveTo OriginAbsolute $(toQArgs args)|]
  Cc args -> [|CurveTo OriginRelative $(toCArgs args)|]
  CC args -> [|CurveTo OriginAbsolute $(toCArgs args)|]
  Ch args -> [|HorizontalTo OriginRelative $(toHArgs args)|]
  CH args -> [|HorizontalTo OriginAbsolute $(toHArgs args)|]
  Cv args -> [|VerticalTo OriginRelative $(toHArgs args)|]
  CV args -> [|VerticalTo OriginAbsolute $(toHArgs args)|]
  Cm args -> [|MoveTo OriginRelative $(toMArgs args)|]
  CM args -> [|MoveTo OriginAbsolute $(toMArgs args)|]
  CZ -> [|EndPath|]

toHArgs :: [HArg] -> Q Exp
toHArgs = listE . map toCoord

toMArgs :: [MArg] -> Q Exp
toMArgs = listE . map toV2

toV2 :: MArg -> ExpQ
toV2 (MArg x y) = [|V2 $(toExp x) $(toExp y)|]

toCoord :: HArg -> ExpQ
toCoord (HArg x) = toExp x

toExp :: E -> ExpQ
toExp = \case
  EDouble d -> [|d|]
  EInt i -> [|fromIntegral i|]
  EVar (Ident a) b -> appsE (dyn a : map toExp b)
  ENeg e -> [| - $(toExp e) |]
  EOp a op b -> liftOp op (toExp a) (toExp b)

liftOp :: Op -> ExpQ -> ExpQ -> ExpQ
liftOp op a b = case op of
  EPlus -> [| $a + $b |]
  EMinus -> [| $a - $b |]
  ETimes -> [| $a * $b |]
  EDiv -> [| $a / $b |]
  EPow -> [| $a ** $b |]

toQArgs :: [QArg] -> Q Exp
toQArgs = listE . map toQArg

toQArg :: QArg -> ExpQ
toQArg (QArg a b) = tupE [toV2 a, toV2 b]

toCArgs :: [CArg] -> ExpQ
toCArgs = listE . map toCArg

toCArg :: CArg -> ExpQ
toCArg (CArg a b c) = tupE [toV2 a, toV2 b, toV2 c]

toAArg :: AArg -> ExpQ
toAArg (AArg rx ry rot large sweep xy) =
  tupE
    [ toExp rx,
      toExp ry,
      toExp rot,
      toExpB large,
      toExpB sweep,
      toV2 xy
    ]

toExpB :: B -> ExpQ
toExpB = \case
  BV (Ident b) -> dyn b
  BI n
    | n == 1 -> [| True |]
    | n == 0 -> [| False |]

toAArgs :: [AArg] -> ExpQ
toAArgs = listE . map toAArg
