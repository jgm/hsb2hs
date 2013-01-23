import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils (info)
import System.Directory
import System.Process
import System.Exit

main :: IO ()
main = defaultMainWithHooks simpleUserHooks{
         hookedPreProcessors = [ppBlobSuffixHandler]
       }

ppBlobSuffixHandler :: PPSuffixHandler
ppBlobSuffixHandler = ("hsb", \_ _ ->
  PreProcessor {
    platformIndependent = True,
    runPreProcessor = mkSimplePreProcessor $ \infile outfile verbosity ->
      do info verbosity $ "Preprocessing " ++ infile ++ " to " ++ outfile
         hsb2hsPath <- findExecutable "hsb2hs"
         case hsb2hsPath of
            Just p  -> rawSystem p [infile, infile, outfile]
            Nothing -> error "hsb2hs is needed to build this program."
         return ()

  })
