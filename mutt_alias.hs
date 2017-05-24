{-# LANGUAGE OverloadedStrings #-}  

{- todo: 
    clean up the terrible main function.
    write promptForAlias
    switch from String to Text ?
-}


-- import Data.Text
import System.Exit
import Data.Char (toLower)
import Data.List (isPrefixOf, find)
import Data.Maybe (fromJust)
import Control.Monad

data Alias = Alias { alias :: Maybe String, name :: String, address :: String }
    deriving (Show, Eq)

aliasFile :: FilePath
aliasFile = "/home/dan/.mutt/aliases.dum.4"

main :: IO ()
main = do
    input <- getContents
    database <- map (last . words) . lines <$> readFile aliasFile
    let maybeSender = fmap fromLine (grepFrom input)
    when (maybeSender == Nothing) (bail input)
    let sender = fromJust maybeSender 
    unless (address sender `elem` database) (writeToFile sender)
    putStr input

bail :: String -> IO ()
bail input = putStr input >> exitFailure

grepFrom :: String -> Maybe String
grepFrom = fmap (fmap removeQuotes) (find (isPrefixOf "From") . lines)

removeQuotes :: String -> String
removeQuotes = filter ((/=) '"')

fromLine :: String -> Alias
fromLine input = Alias Nothing (unwords (init fromline)) (last fromline)
    where fromline = (drop 1 . words) input

-- promptForAlias :: Alias -> IO Alias
-- promptForAlias alias = undefined

writeToFile :: Alias -> IO ()
writeToFile entry = appendFile aliasFile $ showAlias entry

showAlias :: Alias -> String
showAlias entry = unwords (ali:name entry:address entry:["\n"])
    where 
        ali :: String
        ali = case alias entry of
            Nothing -> map toLower $ (filter ((/=) ' ')) (name entry)
            Just str -> str
