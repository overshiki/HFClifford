{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
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

type Env = HS.HashMap String FlowGate

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

-- runParser :: HasCallStack => Parser a -> String -> a 
-- runParser p s = case parse p "" s of 
--   Left bundle -> error (errorBundlePretty bundle)
--   Right r -> r

runParser :: HasCallStack => Parser a -> String -> a 
runParser p s = case v of 
  Left bundle -> error (errorBundlePretty bundle)
  Right r -> r
  where 
    m = runParserT p "" s
    (v, _) = runState m HS.empty

parseInt :: Parser Int 
parseInt = lexeme $ L.signed sc L.decimal

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

-- excludeChar :: [Char]
-- excludeChar = ['(', ')', '{', '}', '[', ']']

parseVar :: Parser String 
parseVar = do 
  -- let 
  --   match = do 
  --     s <- L.charLiteral 
  --     if s `elem` excludeChar 
  --       then return ()
  --       else parseError $ FancyError 0 (Set.singleton (ErrorFail "fail"))
  lexeme $ safeManyTill L.charLiteral (lookAhead (try space1 <|> eof))

parsePauli :: Parser Pauli
parsePauli =  
  try (lstring "x" >> return X) <|>
  try (lstring "y" >> return Y) <|> 
  try (lstring "z" >> return Z) <|> 
  (lstring "i" >> return I)

-- parseFlow :: Parser ()
-- parseFlow = do 
--   lstring "defflow"
--   n <- parseVar
--   lstring "{"

parseGate :: Parser Gate 
parseGate = 
  try parseH
  <|> try parseP 
  <|> try parseM 
  <|> parseCNOT

parseCircuit :: Parser Circuit 
parseCircuit = do 
  cs <- safeManyTill (lexeme parseGate) eof
  return $ Circuit cs

excludePredict :: Parser a -> Parser ()
excludePredict p = lookAhead $ notFollowedBy p 

parseComment :: Parser ()
parseComment = do 
  _ <- safeManyTill L.charLiteral (lstring "#")
  return ()

parseFile :: Parser Circuit 
parseFile = parseComment >> parseCircuit
