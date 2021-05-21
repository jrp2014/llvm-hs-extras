module Main (main) where

import LlvmExtras (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
