module Main (main) where

import Lib


-- From the dd man page:
-- N and BYTES may be followed by the following multiplicative
-- suffixes: c=1, w=2, b=512, kB=1000, K=1024, MB=1000*1000,
-- M=1024*1024, xM=M, GB=1000*1000*1000, G=1024*1024*1024, and so on
-- for T, P, E, Z, Y.  Binary prefixes can be used, too: KiB=K, MiB=M,
-- and so on.  If N ends in 'B', it counts bytes not blocks.

data Units =
  Chars
  | Words
  | Blocks
  | Kibibytes
  | Mebibytes
  | Tebibytes
  | Kilobytes
  | Megabytes
  | Terabytes

instance Show Units where
  show Chars = "c"
  show Words = "w"
  show Blocks = "b"
  show Kibibytes = "K"
  show Mebibytes = "M"
  show Tebibytes = "T"
  show Kilobytes = "KB"
  show Megabytes = "MB"
  show Terabytes = "TB"

main :: IO ()
main = putStrLn "Foo"
