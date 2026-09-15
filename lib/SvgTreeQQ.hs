module SvgTreeQQ where

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
import Prelude hiding (exp)

-- Exp is done
-- type Coord = Double
-- type RPoint = V2 Double
-- data V2 Double = V2 Double Double
-- data Origin = OriginAbsolute  -- uppercase
--  | OriginRelative -- lowercase
-- data PathCommand
-- = MoveTo !Origin ![RPoint] -- | 'M' or 'm' command
-- \| LineTo !Origin ![RPoint] -- | Line to, 'L' or 'l' Svg path command.
-- \| HorizontalTo  !Origin ![Coord] -- | Equivalent to the 'H' or 'h' svg path command.
-- \| VerticalTo    !Origin ![Coord] -- | Equivalent to the 'V' or 'v' svg path command.
-- \| CurveTo  !Origin ![(RPoint, RPoint, RPoint)] -- | Cubic bezier, 'C' or 'c' command
-- \| SmoothCurveTo  !Origin ![(RPoint, RPoint)] -- | Smooth cubic bezier, equivalent to 'S' or 's' command
-- \| QuadraticBezier !Origin ![(RPoint, RPoint)] -- | Quadratic bezier, 'Q' or 'q' command
-- \| SmoothQuadraticBezierCurveTo  !Origin ![RPoint] -- | Quadratic bezier, 'T' or 't' command
-- \| EllipticalArc !Origin ![(Coord, Coord, Coord, Bool, Bool, RPoint)] -- | Eliptical arc, 'A' or 'a' command.
-- \| EndPath -- | Close the path, 'Z' or 'z' svg path command.
-- deriving (Eq, Show)

-- ghci> [exp| 1/2 ** 4 ^ x (2+3) y |]
-- <interactive>:49:6-29: Splicing expression
--     Language.Haskell.TH.Quote.quoteExp exp " 1/2 ** 4 ^ x (2+3) y "
--   ======>
--     EOp
--       (EInt 1) EDiv
--       (EOp
--          (EInt 2) EPow
--          (EOp
--             (EInt 4) EPow
--             (EVar
--                (Ident "x") [EOp (EInt 2) EPlus (EInt 3), EVar (Ident "y") []])))
-- EOp (EInt 1) EDiv (EOp (EInt 2) EPow (EOp (EInt 4) EPow (EVar (Ident "x") [EOp (EInt 2) EPlus (EInt 3),EVar (Ident "y") []])))

-- ghci> :i Exp
-- type Exp :: *
-- data Exp
-- = EDouble Double | EInt Integer | EVar Ident [Exp] | EOp Exp Op Exp
--
-- myLexer :: String -> [Token]
-- data ParseMonad a = Ok a | Bad String

bnfc
  [lbnf|
EDouble.   Exp3 ::= Double;
EInt. Exp3 ::= Integer;
EVar. Exp3 ::= Ident [Exp];
EOp . Exp2 ::= Exp3 Op3 Exp2;
EOp . Exp1  ::= Exp1 Op2 Exp2 ;
EOp . Exp  ::= Exp Op Exp1 ;

separator Exp "";

EPlus. Op ::= "+";
EMinus. Op ::= "-";
ETimes. Op2 ::= "*";
EDiv. Op2 ::= "/";
EPow. Op3 ::= "**";
EPow. Op3 ::= "^";
EPow. Op3 ::= "^^";


_. Exp3 ::= "(" Exp ")" ;
_. Exp2 ::= Exp3 ;
_. Exp1 ::= Exp2 ;
_. Exp  ::= Exp1 ;

 |]
