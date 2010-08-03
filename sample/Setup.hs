import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils (info)
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
         rawSystem "hsb2hs" [infile, outfile]
         return ()
  })
