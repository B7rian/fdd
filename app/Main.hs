module Main (main) where

import Lib

data Units = BinaryUnits | DecimalUnits
data BinaryUnits = Chars | Words | Blocks | Kibibytes | Mebibytes | Tebibytes
data DecimalUnits = Kilobytes | Megabytes | Terabytes

main :: IO ()
main = putStrLn "Foo"
