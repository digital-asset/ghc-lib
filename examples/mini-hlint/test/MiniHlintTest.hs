module MiniHlintTest where

f :: Bool -> Int
f x = 1 + if id (not (not x)) || false then 1 else 2
