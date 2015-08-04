module Util where

import String

formatNumber : Int -> String -> Int -> String
formatNumber digits filler a =
    let
        aDigits = digitsOf a
    in
        String.concat(List.repeat (max (digits - aDigits) 0) filler) ++ toString a

digitsOf : Int -> Int
digitsOf =
    ((+) 1) << round << logBase 10 << toFloat
