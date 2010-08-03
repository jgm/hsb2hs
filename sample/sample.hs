module Main

where
import Data (foo, myblobs)
import Data.ByteString as B
import qualified Data.Map as M

main = do
  B.putStrLn foo
  Prelude.putStrLn $ show $ myblobs

