module Main
where
import qualified Data.ByteString as B
import System.Directory
import System.IO
import System.Exit
import System.Environment
import Data.Char (isSpace)
import Control.Arrow ((&&&), second)
import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.Map as M
import System.FilePath
import Paths_hsb2hs (getDataFileName)

main :: IO ()
main = do
  args <- getArgs
  when ("-h" `elem` args) $ do
    hPutStrLn stderr $
      "Usage: hsb2hs [infile] [outfile] - preprocess infile to outfile\n" ++
      "       hsb2hs -s                 - print default Setup.hs\n" ++
      "       hsb2hs -h                 - this help message\n\n" ++
      "The string `%blob \"path/to/my/file\"' is converted into a string\n" ++
      "literal containing the contents of the file.  This can be treated\n" ++
      "as either a String or (with OverloadedStrings) as a ByteString.\n\n" ++
      "The string `%blobs \"path/to/my/file\"' is converted into a Map\n" ++
      "from FilePaths to Strings or ByteStrings.\n\n" ++
      "To use hsb2hs with Cabal, set Build-type to `Custom' and use the\n" ++
      "output of `hsb2hs -s' for your Setup.hs.  Then you can put blobs\n" ++
      "in Haskell files with the `.hsb' extension, and Cabal will\n" ++
      "automatically preprocess them to `.hs' files."
    exitWith ExitSuccess 
  when ("-s" `elem` args) $ do
    getDataFileName "Setup.sample.hs" >>= readFile >>= putStr
    exitWith ExitSuccess
  let (infile, getin, putout, _opts) = case args of
       (x:y:zs)  -> (x, readFile x, writeFile y, zs)
       [x]       -> (x, readFile x, putStrLn, [])
       []        -> ("stdin", getContents, putStrLn, [])
  getin >>= addBlobs infile >>= putout
  exitWith ExitSuccess

addBlobs :: FilePath -> String -> IO String
addBlobs infile w@('%':'b':'l':'o':'b':'s':xs) =
  case reads (dropWhile isSpace xs) of
       ((p,r):_) -> fileList' p "" >>= \x -> addBlobs infile r >>=
                       return . (show (M.fromList x) ++)
       _         -> error $ "Syntax error in " ++ infile ++ ": " ++
                                takeWhile (/='\n') w
addBlobs infile w@('%':'b':'l':'o':'b':xs) =
  case reads (dropWhile isSpace xs) of
       ((p,r):_) -> B.readFile p >>= \x -> addBlobs infile r >>= return . (show x ++)
       _         -> error $ "Syntax error in " ++ infile ++ ": " ++
                                takeWhile (/='\n') w
addBlobs infile (x:xs) = addBlobs infile xs >>= return . (x:)
addBlobs _ [] = return []

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
