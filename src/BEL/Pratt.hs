{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BEL.Pratt
  ( Env
  , Token(..)
  , Expr(..)
  , expression
  ) where

import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import           Data.Scientific (Scientific)
import           Debug.Trace (trace)

type Env = HM.HashMap String Aeson.Value

data Token =
    TUnit
  | TBool Bool
  | TTrue | TFalse
  | TEq | TNeq | TLte | TGte
  | TJsonpath
  | TIdentifier Text
  | TQuoted Text
  | TParenOpn | TParenCls
  | TPlus | TMinus | TMult | TDiv
  | TNum Scientific
    deriving (Show, Eq)

data Expr =
    VBool   !Bool
  | VObj    !Aeson.Object
  | VArray  !Aeson.Array
  | VNull
  | VString !Text
  | VNum    !Scientific
  | VIdent  !Text

  | Fn  String

  | Neg Expr
  | Eq  Expr Expr
  | Neq Expr Expr
  | Lte Expr Expr
  | Gte Expr Expr
  | App Expr Expr

  | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

  | EPrint Expr
    deriving (Show, Eq)

-- Pratt Parser Implementation

-- "Binding power"
bp :: Token -> Int
bp TEq =     5
bp TNeq =    5
bp TLte =    5
bp TGte =    5
bp TPlus =  10
bp TMinus = 10
bp TMult =  20
bp TDiv =   20
bp _ =       0

-- Null denotation "nud".
nud :: Token -> [Token] -> (Expr, [Token])

nud (TNum n) rest = (VNum n, rest)

nud (TBool b) rest = (VBool b, rest)

nud (TQuoted s) rest = (VString s, rest)

nud (TIdentifier "debug") rest =
    let (e, rest') = expression 0 rest
    in (App (Fn "debug") e, rest')

nud (TIdentifier t) rest = (VIdent t, rest)

nud TJsonpath (TQuoted t : rest) = (App (Fn "jsonpath") (VString t), rest)

nud TParenOpn rest =
    let (e, rest') = expression 0 rest
    in case rest' of
        (TParenCls:rest'') -> (e, rest'')
        _ -> (e, rest')

nud t _ = (VString (Text.pack $ show [t]), [])


-- Left denotation "led".
led :: Token -> Expr -> [Token] -> (Expr, [Token])

led TPlus left rest =
    let (right, rest') = expression 10 rest
    in (Add left right, rest')

led TMinus left rest =
    let (right, rest') = expression 10 rest
    in (Sub left right, rest')

led TMult left rest =
    let (right, rest') = expression 20 rest
    in (Mul left right, rest')

led TDiv left rest =
    let (right, rest') = expression 20 rest
    in (Div left right, rest')

led TEq left rest =
    let (right, rest') = expression 5 rest
    in (Eq left right, rest')

led TNeq left rest =
    let (right, rest') = expression 5 rest
    in (Neq left right, rest')

led TLte left rest =
    let (right, rest') = expression 5 rest
    in (Lte left right, rest')

led TGte left rest =
    let (right, rest') = expression 5 rest
    in (Gte left right, rest')

led t left rest = (left, t:rest)

-- Right binding power (rbp).
expression :: Int -> [Token] -> (Expr, [Token])
expression rbp tokens =
    case tokens of
        [] -> (VString "", [])
        (t:rest) ->
            let (left, rest') = nud t rest
            in h rbp left rest'

    where
    h :: Int -> Expr -> [Token] -> (Expr, [Token])
    h rbp left tokens =
        case tokens of
            [] -> (left, [])
            (t:rest) ->
                if bp t > rbp then
                    let (newLeft, newRest) = led t left rest
                    in h rbp newLeft newRest
                else
                    (left, tokens)
