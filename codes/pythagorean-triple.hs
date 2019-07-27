module PTriple where

satisfy :: Int -> Int -> Int -> Bool
satisfy i j k = k^2 == i^2 + j^2

ans :: Int -> [(Int,Int,Int)]
ans n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], satisfy a b c]
