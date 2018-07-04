-- this program would prabably be better with IO.Strict.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}

import System.Environment (getArgs)
import System.IO
import Text.Read (readMaybe)

path :: FilePath
path = "/sys/class/backlight/intel_backlight/brightness"

max_brightness, min_brightness, increment :: Int
max_brightness = 3750
min_brightness = 1
increment = 300

main :: IO ()
main = do
    args <- getArgs
    handle <- openFile path ReadWriteMode
    current_bright <- read <$> hGetLine handle
    let new_bright = show $ calc args current_bright 
--     putStrLn new_bright
    hSeek handle AbsoluteSeek 0
    hPutStrLn handle (new_bright)
    hSetFileSize handle (fromIntegral (length (new_bright) + 1))
    hClose handle

calc :: [String] -> Int -> Int
calc (x:_) | Just val <- readMaybe x = \_ -> -- if a value is provided, then
    max min_brightness (min max_brightness val) -- ensure 1 < val < 7812
calc ("-d":_) = darken 
calc ("-b":_) = brighten 
calc _ = brighten -- default is brighten

brighten :: Int -> Int
brighten !val = min max_brightness (val + increment)

darken :: Int -> Int
darken !val = max min_brightness (val - increment)
