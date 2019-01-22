module Main where

import Math

v = Vector 1.0 2.0 3.0 4.0
u = Vector 2.0 3.0 4.0 5.0

main :: IO ()
main = print (u + v)
