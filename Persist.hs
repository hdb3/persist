module Persist where
import System.Environment(getEnv)
import System.FilePath.Posix((</>))
import System.Posix.Files(fileExist,removeLink)
import System.Directory(createDirectoryIfMissing)
import System.IO(withFile,hGetContents,IOMode(ReadMode))
import Control.DeepSeq
import Control.Exception(evaluate)


strictRead :: FilePath -> IO String
strictRead fn = withFile fn ReadMode $ \h ->
                       evaluate . force =<< hGetContents h

getStore :: String -> IO FilePath
getStore name = do
    home <- getEnv "HOME"
    let path = home </> ".persist" </> name
    exists <- fileExist path
    if exists
        then
            return path
        else
            fail "can't open store"

destroy :: String -> IO ()
destroy name = do
    path <- getStore name
    removeLink path
 
initialise :: (Read t, Show t) => String -> t -> IO ()
initialise name val = do
    home <- getEnv "HOME"
    createDirectoryIfMissing False ( home </> ".persist" )
    let path = home </> ".persist" </> name
    exists <- fileExist path
    if exists
        then
            fail "can't reinitialise an existing store"
        else
            writeFile path (show val)
 
read :: (Read t, Show t) => String -> IO t
read name = do
    path <- getStore name
    s <- strictRead path
    return $ Prelude.read s

get :: (Read t, Show t) => String -> IO t
get name = do
    path <- getStore name
    s <- strictRead path
    writeFile path ""
    return $ Prelude.read s

put :: (Read t, Show t) => String -> t -> IO ()
put name val= do
    path <- getStore name
    writeFile path (show val)

update :: (Read t, Show t) => String -> (t -> t) -> IO ()
update name f = do
    path <- getStore name
    s <- strictRead path
    writeFile path $ show $ f $ Prelude.read s
