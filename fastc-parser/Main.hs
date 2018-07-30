{-# LANGUAGE DeriveGeneric, FlexibleContexts, TemplateHaskell #-}

module Main (main) where


import Control.Arrow       ((&&&))
import Data.Either         (either)
import Data.Void
import File.Format.Fastc
import System.Environment
import Text.Megaparsec     (Parsec, ParseError, Token, parse, parseErrorPretty')


-- |
-- Main evaluation call.
--
-- Parses files supplied from command line arguments or content of STDIN
main :: IO ()
main = do
     args  <- getArgs
     files <- case args of
                [] -> pure . (const "STDIN" &&& id) <$> getContents
                xs -> sequenceA $ sequenceA . (id &&& readFile) <$> xs
     mapM_ (putStrLn . parseFile) files
  where
    parseFile :: (FilePath, String) -> String
    parseFile (path, contents) = either (parseErrorPretty' contents) show $ parse' fastcStreamParser path contents
      where
        parse' :: Parsec Void s a -> String -> s -> Either (ParseError (Token s) Void) a
        parse' = parse
    
