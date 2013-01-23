module Main
where
import Language.Haskell.Preprocessor
import System.IO.Unsafe (unsafePerformIO)

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

processBlobs :: [Ast] -> [Ast]
processBlobs [] = []
processBlobs (Single (Token Operator _ l "%") :
              Single (Token Variable _ _ kw) :
              Single (Token StringLit _ _ lit) :
              xs) | kw == "blobs" || kw == "blob" =
  (Single (Token StringLit [] l (getLiteral $ stripQuotes lit))) :
     processBlobs xs
   where stripQuotes = reverse . stripLeadingQuote . reverse . stripLeadingQuote
         stripLeadingQuote ('"':ys) = ys
         stripLeadingQuote ys = ys
         getLiteral f = unsafePerformIO $ if kw == "blob"
                                             then show `fmap` B.readFile f
                                             else show `fmap` fileList' f ""
processBlobs (x:xs) =
  (case x of
      Single tok -> Single tok
      Block i l b r n -> Block i l (processBlobs b) r n
      Empty      -> Empty) : processBlobs xs

-- fileList' is taken from Michael Snoyman's file-embed
fileList' :: FilePath -> FilePath -> IO [(FilePath, B.ByteString)]
fileList' realTop top = do
    allContents <- filter notHidden <$> getDirectoryContents (realTop </> top)
    let all' = map ((top </>) &&& (\x -> realTop </> top </> x)) allContents
    files <- filterM (doesFileExist . snd) all' >>=
             mapM (liftPair2 . second B.readFile)
    dirs <- filterM (doesDirectoryExist . snd) all' >>=
            mapM (fileList' realTop . fst)
    return $ concat $ files : dirs

notHidden :: FilePath -> Bool
notHidden ('.':_) = False
notHidden _ = True

liftPair2 :: Monad m => (a, m b) -> m (a, b)
liftPair2 (a, b) = b >>= \b' -> return (a, b')
