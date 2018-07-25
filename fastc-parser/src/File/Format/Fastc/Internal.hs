-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Fastc.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utility functions used for parsing both FASTC file formats.
--
----------------------------------------------------------------------------- 

{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}

module File.Format.Fastc.Internal where

import           Data.CaseInsensitive
import           Data.Functor                      (($>))
import           Data.Maybe                        (catMaybes)
import           Data.Proxy
import qualified Data.Set                   as S
import qualified Text.Megaparsec.Char.Lexer as LEX


import Data.Char              (isSpace)
import Data.List.NonEmpty
import Data.Map               (Map)
import Data.Vector            (Vector)
import Text.Megaparsec
import Text.Megaparsec.Char


-- |
-- Naive representation of a collection taxa sequences
type TaxonSequenceMap  = Map Identifier CharacterSequence


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
-- Parses a line containing the sequence identifier along with an
-- optional conmment which is discarded.
identifierLine :: (MonadParsec e s m, Token s ~ Char) => m Identifier
identifierLine = do
    _ <- char '>'
    _ <- inlineSpace
    x <- identifier 
    _ <- inlineSpace
    _ <- optional (try commentBody <?> commentMessage x)
    _ <- endOfLine <?> lineEndMessage x
    pure x
  where
    commentMessage x = "Invalid comment for following label: '" <> x <> "'"
    lineEndMessage x = "There is no end-of-line after label: '" <> x <> "'"


-- |
-- 'Identifier' of a sequence
identifier :: (MonadParsec e s m, Token s ~ Char) => m Identifier
identifier = some $ satisfy validIdentifierChar


-- |
-- Defines if a 'Char' is valid to be contained within a sequence 'Identifier'
validIdentifierChar :: Char -> Bool
validIdentifierChar c = (not . isSpace) c && c /= '$'


-- |
-- Defines the comment format which can be expected after an identifier
commentBody :: (MonadParsec e s m, Token s ~ Char) => m String
commentBody  = do
    _       <- inlineSpace
    _       <- optional $ char '$'
    _       <- inlineSpace
    content <- many (commentWord <* inlineSpace)
    pure $ unwords content
  where
    -- |
    -- Defines the words of a comment
    commentWord :: (MonadParsec e s m, Token s ~ Char) => m String
    commentWord  = some (satisfy (not . isSpace)) <?> "Non-space characters"


-- |
-- Prepend a single combinator result element to the combinator result of a list
-- of elements.
(<:>)  :: Applicative f => f a -> f [a] -> f [a]
(<:>)  a b = (:)  <$> a <*> b


{-
-- |
-- Concatenate the result of two list producing combinators.
(<++>) :: (Applicative f, Semigroup a) => f a -> f a -> f a
(<++>) a b = (<>) <$> a <*> b
-}


-- |
-- Parse a string-like chunk.
string'' :: forall e s m. (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => String -> m (Tokens s)
string'' = string' . tokensToChunk (Proxy :: Proxy s)


-- |
-- @anythingTill end@ consumes zero or more characters until @end@ is matched,
-- leaving @end@ in the stream.
anythingTill :: MonadParsec e s m => m a -> m [Token s]
anythingTill c = do 
    ahead <- optional . try $ lookAhead c
    case ahead of
      Just _  -> pure []
      Nothing -> somethingTill c


-- |
-- @somethingTill end@ consumes one or more characters until @end@ is matched,
-- leaving @end@ in the stream.
somethingTill :: MonadParsec e s m => m a -> m [Token s]
somethingTill c = do
    _ <- notFollowedBy c
    anyToken <:> anythingTill c


-- |
-- Match any token. Fails only when the stream is empty.
anyToken :: MonadParsec e s m => m (Token s)
anyToken = token Right Nothing


-- |
-- Flexibly parses a 'Double' value represented in a variety of forms.
double :: (MonadParsec e s m, Token s ~ Char) => m Double
double = try real <|> fromIntegral <$> int
  where
     int  :: (MonadParsec e s m, Token s ~ Char) => m Integer
     int  = LEX.signed space LEX.decimal
     real = LEX.signed space LEX.float


-- |
-- Custom 'eol' combinator to account for /very/ old Mac file formats ending
-- lines in a single @\'\\r\'@.
endOfLine :: (Enum (Token s), MonadParsec e s m) => m (Token s)
endOfLine = choice (try <$> [ nl, cr *> nl, cr ]) $> newLineChar
  where
    newLineChar  = enumCoerce '\n'
    carriageChar = enumCoerce '\r'
    nl = tokenMatch newLineChar  $> ()
    cr = tokenMatch carriageChar $> ()


-- |
-- Accepts zero or more Failure messages.
fails :: MonadParsec e s m => [String] -> m a
fails = failure Nothing . S.fromList . fmap Label . catMaybes . fmap nonEmpty


-- |
-- Consumes a whitespace character that is not a newline character.
inlineSpaceChar :: (Enum (Token s), MonadParsec e s m) => m (Token s)
inlineSpaceChar = token captureToken Nothing
  where
    captureToken x
      | isInlineSpace x = Right x
      | otherwise       = Left (Just (Tokens (x:|[])), mempty)
        
    isInlineSpace x = and $
        [ isSpace . enumCoerce
        , (newLineChar  /=)
        , (carriageChar /=)
        ] <*> [x]
        
    newLineChar  = enumCoerce '\n'
    carriageChar = enumCoerce '\r'


-- |
-- Consumes zero or more whitespace characters that are not newline characters.
inlineSpace :: (Enum (Token s), MonadParsec e s m) => m ()
inlineSpace = skipMany inlineSpaceChar


{-
-- |
-- @comment start end@ will parse a /nested/ comment structure which begins with
-- the delimiter @start@ and ends with the delimiter @end@.
--
-- Each opening @start@ must be matched with an @end@ in a proper nested structure.
--
-- *NOTE:* if @start@ and @end@ are not completely disjoint combinators, that is if
-- there exists any string which both @start@ and @end@ can match, the @comment@
-- combinator will not behave as expected. The parser is unable to handle the ambiguity
-- of whether strings matched in the intersection of @start@ and @end@ should represent
-- the beginning of a nested comment structure or the closing of an open comment structure.
--
-- Ensure that the following holds for all `x :: String`:
--
-- > isRight (parse start "" x) /= isRight (parse end "" x)
--
comment :: MonadParsec e s m => m [Token s] -> m [Token s] -> m [Token s]
comment start end = commentDefinition' False
  where
    commentChar    = notFollowedBy (start <|> end) *> anyToken
    commentContent = many commentChar
    commentDefinition' enquote = do
        prefix   <- start
        before   <- commentContent
        comments <- concat <$> many (commentDefinition' True <++> commentContent)
        suffix   <- end
{-        
        after    <- if   enquote
                    then many spaceChar
                    else pure ""
-}
        pure . concat $
          if enquote
          then [ prefix, before, comments, suffix {- , after -} ]
          else [         before, comments         {- , after -} ]


-- |
-- Tries to run a parser on a given file.
-- On a parse success returns the Show value of the parsed result.
-- On a parse failure the nice error string.
runParserOnFile :: Show a => Parsec Void String a -> FilePath -> IO String
runParserOnFile parser filePath = either (parseErrorPretty :: ParseError Char Void -> String) show . parse parser filePath <$> readFile filePath


-- |
-- Runs the supplied parser on the input stream with default error types.
-- Useful for quick tests in GHCi.
parseWithDefaultErrorType :: Parsec Void s a -> s -> Either (ParseError (Token s) Void) a
parseWithDefaultErrorType c = parse c "" 
-}


-- Takes a 'Stream' of 'Char's and returns a String
-- with EOL sequences standardized to the Unix EOL sequence.
--
--  * @"\\r\\n"@ -> @\'\\n\'@ (Windows EOL sequence)
--
--  * @"\\r"@ -> @\'\\n\'@ (Old Mac EOL sequence
--
--  * @"\\n"@ -> @\'\\n\'@ (Unix EOL sequence)
--unifyNewlines :: Stream s Char => s -> String
--unifyNewlines xs =
--  case (curr, next) of
--    (Nothing  , _        ) -> []
--    (Just '\r', Just '\n') -> '\n' : maybe [] unifyNewlines less
--    (Just '\r', _        ) -> '\n' : maybe [] unifyNewlines more
--    (Just tok , Just _   ) -> tok  : maybe [] unifyNewlines more
--    (Just tok , Nothing  ) -> [tok]
--  where
--    (curr, more) = seqTuple $ uncons xs
--    (next, less) = seqTuple $ uncons =<< more
--    seqTuple may = case may of
--                     Just (x,y) -> (Just x, Just y)
--                     Nothing    -> (Nothing,Nothing)
                     

-- |
-- Convert one Enum to another through the Int value.
enumCoerce :: (Enum a, Enum b) => a -> b
enumCoerce = toEnum . fromEnum


-- |
-- Matches a single token.
tokenMatch :: (MonadParsec e s m) => Token s -> m (Token s)
tokenMatch tok = token testToken Nothing
  where
    testToken x
      | tok == x  = Right x
      | otherwise = Left (Just (Tokens (x:|[])), mempty)
