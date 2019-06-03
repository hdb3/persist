{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import qualified Persist
import System.Environment(getArgs)
import Data.IP
import Data.Word(byteSwap32)
import Data.Maybe(isJust,fromJust,catMaybes)
import Data.List((\\),sort,elem)
import Control.Monad(when,unless)
import Text.Read(readMaybe)
import System.Exit(die)

store = "IP"
main = do
    args <- getArgs
    if null args
        then
            usage
        else
            case head args of
                "init"    -> Main.init (tail args)
                "release" -> release (tail args)
                "acquire" -> acquire (tail args)
                "destroy" -> Persist.destroy store
                "show"    -> display
                _         -> usage

usage = die $ unlines [ "ipstore init <start address> <pool size>"
                      , "ipstore acquire <count>"
                      , "ipstore release <address> <address> ..."
                      , "ipstore show"
                      , "ipstore destroy"
                      ]

init args = do
    unless ( 2 == length args ) usage
    let startAddress = readMaybe (head args) :: Maybe IPv4
    unless ( isJust startAddress ) usage
    let count = readMaybe (args!!1) :: Maybe Int
    unless ( isJust count ) usage

    let pool :: ([IPv4],[IPv4])  = ( map (fromHostAddress . byteSwap32) $ take (fromJust count) $ [byteSwap32 $ toHostAddress (fromJust startAddress) .. ] , [])
    Persist.initialise store pool
    putStrLn "address range initialised"
    
display = do
    (free,used)  :: ([IPv4],[IPv4]) <- Persist.read store
    print (free,used)
    
acquire args = do
    when (null args) usage
    let count = readMaybe (head args) :: Maybe Int
    unless ( isJust count ) usage
    (free,used)  :: ([IPv4],[IPv4]) <- Persist.read store
    if (fromJust count) > length free
        then
            die "acquire fail: not enough IPs in pool"
        else do
            let acquiredIPs = take (fromJust count) free
            Persist.put store ( sort $ free \\ acquiredIPs , sort $ used ++ acquiredIPs)
            putStrLn $ unwords $ map show acquiredIPs
    
release args = do
    when (null args) usage
    let releasedIPs :: [IPv4] = catMaybes $ map readMaybe args
    unless ( length args == length releasedIPs ) usage
    (free,used)  :: ([IPv4],[IPv4]) <- Persist.read store
    if releasedIPs `isSubsetOf` used
        then
            Persist.put store ( sort $ free ++ releasedIPs , sort $ used \\ releasedIPs)
        else
            die "release fail: IPs were not in use"
    where
        isSubsetOf [] _ = True
        isSubsetOf (a:ax) super = a `elem` super && ax `isSubsetOf` super 
