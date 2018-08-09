-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Fastc.Parser
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utility functions used for parsing both FASTA & FASTC file formats.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ApplicativeDo, FlexibleContexts, TypeFamilies #-}

module File.Format.Fastc.Parser 
  ( CharacterSequence
  , FastcParseResult
  , FastcSequence(..)
  , Identifier
  , Symbol
  , fastcStreamParser
  ) where


import           Data.Char                   (isSpace)
import           Data.Foldable
import           Data.Functor                (void)
import           Data.List.NonEmpty          (NonEmpty(..), some1)
import           Data.Vector                 (Vector)
import qualified Data.Vector          as V
import           Text.Megaparsec
import           Text.Megaparsec.Char hiding (spaceChar)
import           Prelude              hiding (sequence)


-- |
-- Unique identifier for a taxa 
type Identifier        = String


-- |
-- Component of a phylogenetic character
type Symbol            = String


-- |
-- Indexed sequences of 'Symbol's with possible abiguity at an index
type CharacterSequence = Vector (NonEmpty Symbol)



-- |
-- Unconverted result of a fastc parse
type FastcParseResult = NonEmpty FastcSequence


-- |
-- Pairing of taxa label with an unconverted sequence
data FastcSequence
   = FastcSequence
   { fastcLabel   :: Identifier
   , fastcSymbols :: CharacterSequence
   } deriving (Eq)


instance Show FastcSequence where

    show (FastcSequence i s) = unlines
      [ i <> ":"
      , unwords $ (\x -> "["<>x<>"]") . unwords . toList <$> toList s
      ]


-- |
-- Consumes a stream of 'Char's and parses the stream into a 'FastcParseResult'
fastcStreamParser :: (MonadParsec e s m, Token s ~ Char) => m FastcParseResult
fastcStreamParser = file <* eof


-- |
-- The full FASTC file grammar
file :: (MonadParsec e s m, Token s ~ Char) => m (NonEmpty FastcSequence)
file = maybespace *> some1 identifierLine
  where
    identifierLine = do
        _ <- char '>'
        i <- identifier
        _ <- idEnd
        s <- sequence
        pure $ FastcSequence i s


-- |
-- 'Identifier' of a sequence
identifier :: (MonadParsec e s m, Token s ~ Char) => m Identifier
identifier = spacePad *> some validChar <* spacePad


-- |
-- The way in which an identifier line can be terminated
idEnd :: (MonadParsec e s m, Token s ~ Char) => m ()
idEnd = char '\n' *> maybespace <|> comment *> maybespace


-- |
-- An inline comment of the FASTC file
comment :: (MonadParsec e s m, Token s ~ Char) => m String
comment = do
    _ <- char ';'
    c <- some inlineChar
    v <- optional $ lookAhead eof
    _ <- case v of
           Nothing -> void $ char '\n'
           Just _  -> pure ()
    pure c


-- |
-- A sequence of symbols
sequence :: (MonadParsec e s m, Token s ~ Char) => m CharacterSequence
sequence = do
    _  <- maybespace
    e  <- element
    es <- many $ try (whitespace *> element)
    _  <- maybespace
    pure . V.fromList $ e:es


-- |
-- An element of the symbol sequence
element :: (MonadParsec e s m, Token s ~ Char) => m (NonEmpty String)
element = (pure <$> symbol) <|> ambiguitygroup
  where
    ambiguitygroup = do
        _  <- char '['
        _  <- maybespace
        x  <- symbol
        xs <- symbolList
        _  <- maybespace
        _  <- char ']'
        pure (x:|xs)


-- |
-- A symbol of the alphabet contained in a symbol sequence
symbol :: (MonadParsec e s m, Token s ~ Char) => m String
symbol = some validChar


-- |
-- A list of symbols, possibly empty
symbolList :: (MonadParsec e s m, Token s ~ Char) => m [String]
symbolList = many $ try (whitespace *> symbol)

  
-- |
-- Defines whitespace to be consumed and discarded
maybespace :: (MonadParsec e s m, Token s ~ Char) => m ()
maybespace = void (many spacing)


-- |
-- Defines whitespace to be consumed and discarded
whitespace :: (MonadParsec e s m, Token s ~ Char) => m ()
whitespace = void (some1 spacing)


-- |
-- Defines a fragment of whitespace to be consumed and discarded
spacing :: (MonadParsec e s m, Token s ~ Char) => m ()
spacing = (void . some1 . satisfy) isSpace <|> void comment


-- |
-- Defines if a 'Char' is a space but not a newline
spacePad :: (MonadParsec e s m, Token s ~ Char) => m String
spacePad = many spaceChar


-- |
-- Defines if a 'Char' is a space but not a newline
spaceChar :: (MonadParsec e s m, Token s ~ Char) => m Char
spaceChar = satisfy (\x -> isSpace x && x /= '\n')


-- |
-- Defines if a 'Char' is not a new line character
inlineChar :: (MonadParsec e s m, Token s ~ Char) => m Char
inlineChar = satisfy (/= '\n')


-- |
-- Defines if a 'Char' is valid
validChar :: (MonadParsec e s m, Token s ~ Char) => m Char
validChar = satisfy charCriterion
  where
    charCriterion :: Char -> Bool
    charCriterion c = and $
        [ not . isSpace
        , (/= ';')
        , (/= '>')
        , (/= '[')
        , (/= ']')
        ] <*> [c]
