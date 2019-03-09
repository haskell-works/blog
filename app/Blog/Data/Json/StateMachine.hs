module Blog.Data.Json.StateMachine where

import Data.Char

{-# ANN module ("HLint: ignore Redundant guard"  :: String) #-}

data State = InJson | InValue | InString | InEscape
  deriving (Eq, Enum, Bounded, Show)

isOpen :: Char -> Bool
isOpen c = c == '{' || c == '['

isClose :: Char -> Bool
isClose c = c == ']' || c == '}'

isDelim :: Char -> Bool
isDelim c = c == ':' || c == ','

isDoubleQuote :: Char -> Bool
isDoubleQuote c = c == '"'

isBackSlash :: Char -> Bool
isBackSlash c = c == '\\'

stateMachine :: Char -> State -> State
stateMachine c InJson   | isOpen        c =  InJson
stateMachine c InJson   | isClose       c =  InJson
stateMachine c InJson   | isDelim       c =  InJson
stateMachine c InJson   | isAlphaNum    c =  InValue
stateMachine c InJson   | isDoubleQuote c =  InString
stateMachine _ InJson   | otherwise       =  InJson
stateMachine c InString | isDoubleQuote c =  InJson
stateMachine c InString | isBackSlash   c =  InEscape
stateMachine _ InString | otherwise       =  InString
stateMachine _ InEscape | otherwise       =  InString
stateMachine c InValue  | isOpen        c =  InJson
stateMachine c InValue  | isClose       c =  InJson
stateMachine c InValue  | isDelim       c =  InJson
stateMachine c InValue  | isAlphaNum    c =  InValue
stateMachine c InValue  | isDoubleQuote c =  InString
stateMachine _ InValue  | otherwise       =  InJson
