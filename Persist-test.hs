{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import qualified Persist

store = "simple"
main = do
    Persist.initialise store $ ( [0..9] :: [Int] )
    x0 :: [Int] <- Persist.read store
    putStr "x0" ; print x0
    x0' :: [Int] <- Persist.read store
    putStr "x0'" ; print x0'
    x1 :: [Int] <- Persist.get store
    putStr "x1" ; print x1
    Persist.put store $ reverse x1
    x2 :: [Int] <- Persist.read store
    putStr "x2" ; print x2
    Persist.update store ( map (10+) . tail )
    x3 :: [Int] <- Persist.read store
    putStr "x3" ; print x3
