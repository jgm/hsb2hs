module Main

where
import Data (foo, myblobs)
import Data.ByteString.Char8 as B
import qualified Data.Map as M

main = do
  B.putStrLn foo
  Prelude.putStrLn $ show $ myblobs

