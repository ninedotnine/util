{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- todo:
    write promptForAlias
    switch from String to Text ?
-}


-- import Data.Text
import System.Exit (exitFailure)
import Data.Char (toLower, isSpace)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isPrefixOf, find)
import Control.Monad (unless)

data Alias = Alias {
    alias :: Maybe String,
    name :: String,
    address :: Address
    } deriving (Show, Eq)

newtype Address = Address {
    from_address :: String
    } deriving (Show, Eq)

aliasFile :: FilePath
aliasFile = "/home/dan/.mutt/aliases"

main :: IO ()
main = do
    input <- getContents
    database :: [Address] <- readFile aliasFile <&> parse
    case grepFrom input <&> fromLine of
        Nothing -> bail input
        Just (sender :: Alias) -> do
            unless ((address sender & normalize) `elem` database)
                   (save sender)
            putStr input

parse :: String -> [Address]
parse = lines <&> filter (not . all isSpace) <&> map addr
    where
        addr = words <&> last <&> Address <&> normalize

normalize :: Address -> Address
normalize = from_address <&> map toLower <&> Address

bail :: String -> IO ()
bail input = putStr input >> exitFailure

grepFrom :: String -> Maybe String
grepFrom = lines <&> find (isPrefixOf "From") <&> fmap removeQuotes
    where
        removeQuotes :: String -> String
        removeQuotes = filter ((/=) '"')

fromLine :: String -> Alias
fromLine input = Alias
    Nothing
    (init fromline & unwords)
    (last fromline & Address)
        where fromline = (drop 1 . words) input

-- promptForAlias :: Alias -> IO Alias
-- promptForAlias alias = undefined

save :: Alias -> IO ()
save entry = appendFile aliasFile $ "alias " ++ showAlias entry

showAlias :: Alias -> String
showAlias entry = unwords (ali:name entry:addr:["\n"])
    where
        ali :: String
        ali = case alias entry of
            Nothing -> (filter ((/=) ' ')) (name entry) & map toLower
            Just str -> str
        addr = address entry & from_address
