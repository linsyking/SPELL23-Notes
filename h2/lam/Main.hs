module Main where
import qualified Deep

main :: IO ()
-- main = putStrLn "Hello, Haskell!"
main = (print . Deep.beta) Deep.idapp
