{-# LANGUAGE DoAndIfThenElse, FlexibleContexts #-}

module Main
  ( main
  ) where


import Control.Arrow     ((&&&))
import Data.Either
import Data.Map          (Map, fromList, toList)
import Data.Void
import File.Format.Fastc
import System.Directory
import Test.Tasty        (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
import Text.Megaparsec


main :: IO ()
main = testSuite >>= defaultMain 


testSuite :: IO TestTree
testSuite = testGroup "fastcStreamParser" <$> sequence [validFastcFiles, invalidFastcFiles]


validFastcFiles :: IO TestTree
validFastcFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "example-files/valid"
    validateFileContents   = testGroup "Valid files" . fmap success . toList
    success (path,content) = testCase (show path) $ parseSuccess fastcStreamParser content


invalidFastcFiles :: IO TestTree
invalidFastcFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "example-files/invalid"
    validateFileContents   = testGroup "Invalid files" . fmap failure . toList
    failure (path,content) = testCase (show path) $ parseFailure fastcStreamParser content


parseFailure :: Parsec Void String a -> String -> Assertion
parseFailure parser input =
    assertBool ("Should have failed to parse input: " <> show input) $ isLeft result
  where
    result = parse parser "" input


parseSuccess :: Parsec Void String a -> String -> Assertion
parseSuccess parser input =
    case result of
      Left  x -> assertFailure $ parseErrorPretty x
      Right _ -> () @=? ()
  where
    result = parse parser "" input


-- |
-- Gets all the given files and thier contents in the specified directory
getFileContentsInDirectory :: FilePath -> IO (Map FilePath String)
getFileContentsInDirectory path = do
    exists <- doesDirectoryExist path
    if not exists
    then pure mempty
    else do
        files  <- filter isFile <$> getDirectoryContents path
        sequence . fromList $ (id &&& readFile) . withPath <$> files
  where
    isFile = not . all (=='.')
    withPath file
      | last path /= '/' = path <> "/" <> file
      | otherwise        = path <> file
