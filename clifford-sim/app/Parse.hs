{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use <&>" #-}
module Parse where
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
-- import System.IO
import Control.Applicative
import GHC.Stack (HasCallStack)
import Control.Monad.State.Lazy

import qualified Data.HashMap.Strict as HS
import qualified Data.Set as Set
import Ast
import Data.Maybe

-- type Parser = Parsec Void String
type Parser = ParsecT Void String (State Env)

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "\\")
  (L.skipBlockComment "(*" "*)")

-- cosume all the white spaces following a Parser
-- white spaces include: " " "\n" "\t"
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lstring :: String -> Parser String
lstring = lexeme . string

safeManyTill :: MonadParsec e s f => f a -> f b -> f [a]
safeManyTill p end = go
  where
    go = try ([] <$ end) <|> liftA2 (:) p go

manyBetween :: Parser a -> Parser a -> Parser String
manyBetween s e = s *> safeManyTill L.charLiteral e

runParser :: HasCallStack => Parser a -> String -> a
runParser p s = case v of
  Left bundle -> error (errorBundlePretty bundle)
  Right r -> r
  where
    m = runParserT p "" s
    (v, _) = runState m HS.empty

parseInt :: Parser Int
parseInt = lexeme $ L.signed sc L.decimal

updateFlowDef :: String -> FlowDef -> Parser ()
updateFlowDef n flow = do
  d <- get
  if n `notElem` HS.keys d
    then do
      let nd = HS.insert n flow d
      put nd
    else parseError $ FancyError 0 (Set.singleton (ErrorFail "fail"))

checkGlowGateExists :: String -> Parser Bool 
checkGlowGateExists n = do 
  d <- get 
  return $ n `elem` HS.keys d

parseH :: Parser Gate
parseH = do
  lstring "h"
  index <- parseInt
  return $ H (QIndex index)

parseP :: Parser Gate
parseP = do
  lstring "p"
  index <- parseInt
  return $ P (QIndex index)

parseCNOT :: Parser Gate
parseCNOT = do
  lstring "c"
  source <- parseInt
  target <- parseInt
  return $ CNOT (QIndex source) (QIndex target)

parseM :: Parser Gate
parseM = do
  lstring "m"
  index <- parseInt
  return $ M (QIndex index)

parseFlow :: Parser Gate 
parseFlow = do 
  v <- parseVar
  isExists <- checkGlowGateExists v 
  if isExists
    then do 
      index <- parseInt
      return $ Flow v (QIndex index)
    else parseError $ FancyError 0 (Set.singleton (ErrorFail ("flow: " ++ v ++ " does not exists")))

parseVar :: Parser String
parseVar = do
  lexeme $ safeManyTill L.charLiteral (lookAhead (try space1 <|> eof))

parsePauli :: Parser Pauli
parsePauli =
  try (lstring "x" >> return X) <|>
  try (lstring "y" >> return Y) <|>
  try (lstring "z" >> return Z) <|>
  (lstring "i" >> return I)

parsePauliArrow :: Parser (Pauli, Pauli)
parsePauliArrow = do
  p1 <- parsePauli
  lstring "->"
  p2 <- parsePauli
  lstring ";"
  return (p1, p2)

parseFlowDef :: Parser ()
parseFlowDef = do
  lstring "defflow"
  n <- parseVar
  lstring "{"
  cs <- safeManyTill (lexeme parsePauliArrow) (lstring "}")
  let flow = FlowDef cs
  updateFlowDef n flow

parseGate :: Parser Gate
parseGate =
  try parseFlow
  <|> try parseH
  <|> try parseP
  <|> try parseM
  <|> parseCNOT

parseFlowDefOrGate :: Parser (Maybe Gate)
parseFlowDefOrGate =
  try (parseFlowDef >> return Nothing) <|>
  (parseGate >>= (return . Just))

parseCircuit :: Parser (Env, Circuit)
parseCircuit = do
  cs <- safeManyTill (lexeme parseFlowDefOrGate) eof
  let ncs = catMaybes cs
  d <- get
  return (d, Circuit ncs)

excludePredict :: Parser a -> Parser ()
excludePredict p = lookAhead $ notFollowedBy p

parseComment :: Parser ()
parseComment = do
  _ <- safeManyTill L.charLiteral (lstring "#")
  return ()

parseFile :: Parser (Env, Circuit)
parseFile = parseComment >> parseCircuit
