-- Works for single, all-lowercase words.

module Cipher where

import Data.Char

zero = ord 'a'

shift n x =
  ((x + n - zero) `mod` 26) + zero

caesar n str =
  map shifter str
  where shifter = chr . shift n . ord

uncaesar n =
  caesar (-n)
