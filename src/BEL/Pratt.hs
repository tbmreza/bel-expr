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
    VBool !Bool
  -- | VObj  -- https://hackage.haskell.org/package/aeson-2.2.3.0/docs/Data-Aeson.html#t:Value
  | VString !Text
  | VNum  !Scientific

  | Fn   String

  | Neg Expr
  | Eq  Expr Expr
  | Neq Expr Expr

  | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

  | App Expr Expr
  | EPrint Expr
  deriving (Show, Eq)

-- Pratt Parser Implementation

-- "Binding power"
bp :: Token -> Int
bp TEq = 5
bp TNeq = 5
bp TLte = 5
bp TGte = 5
bp TPlus = 10
bp TMinus = 10
bp TMult = 20
bp TDiv = 20
bp _ = 0

-- Null denotation "nud".
nud :: Env -> Token -> [Token] -> (Expr, [Token])

nud _ (TNum n) rest = (VNum n, rest)

nud _ (TBool b) rest = (VBool b, rest)

nud _ (TQuoted s) rest = (VString s, rest)

nud env (TIdentifier "debug") rest =
    let (e, rest') = expression env 0 rest
    in (App (Fn "debug") e, rest')

nud env (TIdentifier t) rest =
    case HM.lookup (Text.unpack t) env of
        Just (Aeson.Bool v) ->   trace "nud Bool"   (VBool v, rest)
        Just (Aeson.String v) -> trace "nud String" (VString v, rest)
        Just (Aeson.Number v) -> trace "nud Number" (VNum v, rest)

        -- Just av -> (trace ("t: " ++ show t ++ ";av: " ++ show av) (VString "RESP_BODY", rest))
        -- ??: why debug EPrint makes sense here in nud
        Just _av ->              (VString t, rest)
        Nothing ->               trace "nud Nothing" (VString t, rest)

nud env TJsonpath (TQuoted t : rest) = (App (Fn "jsonpath") (VString t), rest)

nud env TParenOpn rest =
    let (e, rest') = expression env 0 rest
    in case rest' of
        (TParenCls:rest'') -> (e, rest'')
        _ -> (e, rest')

nud _ t _ = (VString (Text.pack $ show [t]), [])


-- Left denotation "led".
led :: Env -> Token -> Expr -> [Token] -> (Expr, [Token])

led env TPlus left rest =
    let (right, rest') = expression env 10 rest
    in (Add left right, rest')

led env TMinus left rest =
    let (right, rest') = expression env 10 rest
    in (Sub left right, rest')

led env TMult left rest =
    let (right, rest') = expression env 20 rest
    in (Mul left right, rest')

led env TDiv left rest =
    let (right, rest') = expression env 20 rest
    in (Div left right, rest')

led env TEq left rest =
    let (right, rest') = expression env 5 rest
    in (Eq left right, rest')

led env TNeq left rest =
    let (right, rest') = expression env 5 rest
    in (Neq left right, rest')

led _ t left rest = (left, t:rest)

-- Right binding power (rbp).
expression :: Env -> Int -> [Token] -> (Expr, [Token])
expression env rbp tokens =
    case tokens of
        [] -> (VString "", [])
        (t:rest) ->
            let (left, rest') = nud env t rest
            in loop env rbp left rest'

loop :: Env -> Int -> Expr -> [Token] -> (Expr, [Token])
loop env rbp left tokens =
    case tokens of
        [] -> (left, [])
        (t:rest) ->
            if bp t > rbp
            then
                let (newLeft, newRest) = led env t left rest
                in loop env rbp newLeft newRest
            else (left, tokens)
