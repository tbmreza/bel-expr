{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module BEL.Pratt
    where

import qualified Data.HashMap.Strict as HM
-- import           Control.Lens
-- import           Control.Lens.At (at)

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import           Data.Scientific (Scientific)

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (Response (..), Request (..))

-- type Env = HM.HashMap String Aeson.Value

data Env = Env
  { responseCopy :: Response ByteString
  , requestCopy ::  Request
  , bindings ::     HM.HashMap String Aeson.Value
  }

data Token =
    TUnit
  | TTrue | TFalse
  | TEq | TNeq | TLte | TGte
  | TJsonpath | TDebug | TCopy
  | TIdentifier Text
  | TQuoted Text
  | TParenOpn | TParenCls
  | TPlus | TMinus | TMult | TDiv
  | TNum Scientific
  | THeader | TExists | TNot
    deriving (Show, Eq)

data TracePropagation =
    TracePropagationDefault
  | TracePropagationExpr Expr
  | TracePropagationClipboard
    deriving (Show, Eq)

data Expr where
    VTrace  :: Expr -> TracePropagation -> Expr
    VBool   :: !Bool         -> Expr
    VObj    :: !Aeson.Object -> Expr
    VArray  :: !Aeson.Array  -> Expr
    VNull   :: Expr
    VString :: !Text         -> Expr
    VNum    :: !Scientific   -> Expr
    VIdent  :: !Text         -> Expr

    ENeg :: Expr -> Expr
    EEq  :: Expr -> Expr -> Expr
    ENeq :: Expr -> Expr -> Expr
    ELte :: Expr -> Expr -> Expr
    EGte :: Expr -> Expr -> Expr

    EAdd :: Expr -> Expr -> Expr
    ESub :: Expr -> Expr -> Expr
    EMul :: Expr -> Expr -> Expr
    EDiv :: Expr -> Expr -> Expr

    EDebug    :: Expr -> Expr
    ECopy     :: Expr -> Expr
    EJsonpath :: Expr -> Expr

    ---------------------------------------------------------------------------
    -- Keywords trickled from query language employed by hurl (xpath-inspired).
    ---------------------------------------------------------------------------
    EHeaderNotExists :: Expr -> Expr  -- arg is Expr reducible to headers key
    EHeaderExists    :: Expr -> Expr

  -- >debug data
  -- >debug "$"
  -- >debug "$.maybe.null.at.path"
  -- >debug "$.ill.path.."

-- data Checked
-- data Unchecked
--
-- data JsonpathStr state where
--     JsonpathStrNew :: String -> JsonpathStr Unchecked
--     JsonpathStrOk  :: String -> JsonpathStr Checked
--
-- deriving instance Show (JsonpathStr state)
-- deriving instance Eq   (JsonpathStr state)
deriving instance Show Expr
deriving instance Eq   Expr

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

nud (TNum n) rest =    (VNum n, rest)
nud TTrue rest =       (VBool True, rest)
nud TFalse rest =      (VBool False, rest)
nud (TQuoted s) rest = (VString s, rest)

nud TMinus rest =
    let (e, rest') = pratt 100 rest
    in (ENeg e, rest')

-- Expr will evaluate to true with printing side-effect. Typically used in
-- [Asserts] block though valid everywhere else in hhs.
nud TDebug rest =
    let (e, rest') = pratt 0 rest
    in (EDebug e, rest')

nud TCopy rest =
    let (e, rest') = pratt 0 rest
    -- in (EDebug e, rest')
    in (ECopy e, rest')

nud (TIdentifier t) rest = (VIdent t, rest)

nud TJsonpath (TQuoted t : rest) =
    (EJsonpath (VString t), rest)

-- Expr will evaluate to a bool after consulting to Env.
nud THeader [TQuoted t, TNot, TExists] =
    (EHeaderNotExists (VString t), [])

nud THeader [TQuoted t, TExists] =
    (EHeaderExists (VString t), [])

nud TParenOpn rest =
    let (e, rest') = pratt 0 rest
    in case rest' of
        (TParenCls:rest'') -> (e, rest'')
        _ -> (e, rest')

-- ??: in interpreters and evaluators context, decision tree for when to panic,
-- when to null. different levels are parser (with binding powers), ast
-- matching, and user facing apis.
nud t rest = (VString (Text.pack $ (show [t] ++ show [rest])), [])


-- Left denotation "led".
led :: Token -> Expr -> [Token] -> (Expr, [Token])

led TPlus left rest =
    let (right, rest') = pratt 10 rest
    in (EAdd left right, rest')

led TMinus left rest =
    let (right, rest') = pratt 10 rest
    in (ESub left right, rest')

led TMult left rest =
    let (right, rest') = pratt 20 rest
    in (EMul left right, rest')

led TDiv left rest =
    let (right, rest') = pratt 20 rest
    in (EDiv left right, rest')

led TEq left rest =
    let (right, rest') = pratt 5 rest
    in (EEq left right, rest')

led TNeq left rest =
    let (right, rest') = pratt 5 rest
    in (ENeq left right, rest')

led TLte left rest =
    let (right, rest') = pratt 5 rest
    in (ELte left right, rest')

led TGte left rest =
    let (right, rest') = pratt 5 rest
    in (EGte left right, rest')

led t left rest = (left, t:rest)

-- Right binding power (rbp).
pratt :: Int -> [Token] -> (Expr, [Token])
pratt rbp tokens =
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
