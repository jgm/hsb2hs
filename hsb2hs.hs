module Main
where
import Language.Haskell.Preprocessor

import qualified Data.ByteString as B
import System.Directory
import System.Environment (getArgs)
import Control.Arrow ((&&&), second)
import Control.Applicative ((<$>))
import Control.Monad
import System.FilePath

main :: IO ()
main = do
  args <- getArgs
  transform blobExtension args

blobExtension :: Extension
blobExtension = base{
    transformer = processBlobs
}

processBlobs :: [Ast] -> IO [Ast]

processBlobs [] = return []

processBlobs (Single (Token Operator _ l "%") :
              Single (Token Variable _ _ kw) :
              Single (Token StringLit _ _ lit) :
              xs) | kw == "blobs" || kw == "blob" = do
  let f = stripQuotes lit
  t <- if kw == "blob"
          then show `fmap` B.readFile f
          else show `fmap` fileList' f ""
  rest <- processBlobs xs
  return $ (Single (Token StringLit [] l t)) : rest
 where stripQuotes = reverse . stripLeadingQuote . reverse . stripLeadingQuote
       stripLeadingQuote ('"':ys) = ys
       stripLeadingQuote ys = ys

processBlobs (Block i l b r n : xs) = do
  bs <- processBlobs b
  rest <- processBlobs xs
  return $ Block i l bs r n : rest

processBlobs (x : xs) = do
  rest <- processBlobs xs
  return $ x : rest

-- fileList' is taken from Michael Snoyman's file-embed
fileList' :: FilePath -> FilePath -> IO [(FilePath, B.ByteString)]
fileList' realTop top = do
    allContents <- filter isReal <$> getDirectoryContents (realTop </> top)
    let all' = map ((top </>) &&& (\x -> realTop </> top </> x)) allContents
    files <- filterM (doesFileExist . snd) all' >>=
             mapM (liftPair2 . second B.readFile)
    dirs <- filterM (doesDirectoryExist . snd) all' >>=
            mapM (fileList' realTop . fst)
    return $ concat $ files : dirs

isReal :: FilePath -> Bool
isReal "."  = False
isReal ".." = False
isReal _    = True

liftPair2 :: Monad m => (a, m b) -> m (a, b)
liftPair2 (a, b) = b >>= \b' -> return (a, b')
