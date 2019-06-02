module Persist where
import Control.Monad(liftM)
import System.Environment(getEnv)
import System.FilePath.Posix((</>))
import System.Posix.Files(fileExist)

initialise :: (Read t, Show t) => String -> t -> IO ()
initialise name val = do
    home <- getEnv "HOME"
    let path = home </> name
    exists <- fileExist path
    if exists
        then
            fail "can't reinitilaise an existing store"
        else
            writeFile path (show val)
 
read :: (Read t, Show t) => String -> IO t
read name = do
    home <- getEnv "HOME"
    let path = home </> name
    exists <- fileExist path
    if exists
        then do
            s <- readFile path
            return $ Prelude.read s
        else
            fail "can't open store"

get :: (Read t, Show t) => String -> IO t
get name = do
    home <- getEnv "HOME"
    let path = home </> name
    exists <- fileExist path
    if exists
        then do
            s <- readFile path
            writeFile path ""
            return $ Prelude.read s
        else
            fail "can't open store"

put :: (Read t, Show t) => String -> t -> IO ()
put name val= do
    home <- getEnv "HOME"
    let path = home </> name
    exists <- fileExist path
    if exists
        then
            writeFile path (show val)
        else
            fail "can't open store"

update :: (Read t, Show t) => String -> (t -> t) -> IO ()
--update p f = put p (f (get p))
--update p f = get p >>= liftM f >>= put p

update name f = do
    home <- getEnv "HOME"
    let path = home </> name
    exists <- fileExist path
    if exists
        then do
            s <- readFile path
            writeFile path $ show $ f $ Prelude.read s
        else
            fail "can't open store"
