#!/bin/runghc
-- this program would prabably be better with IO.Strict.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}

import System.Environment (getArgs)
import System.IO
import Text.Read (readMaybe)

path :: FilePath
path = "/sys/class/backlight/intel_backlight/brightness"

max_brightness, increment :: Int
max_brightness = 7812
increment = 500

main :: IO ()
main = do
-- only opens the file once but is uglier...
    args <- getArgs
    handle <- openFile path ReadWriteMode
    current_bright <- read <$> hGetLine handle
    let new_bright = show $ calc current_bright args
--     putStrLn new_bright
    hSeek handle AbsoluteSeek 0
    hPutStrLn handle (new_bright)
    hSetFileSize handle (fromIntegral (length (new_bright) + 1))
    hClose handle
{-
-- this implementation looks nice, but i think it causes a race condition... 
    args <- getArgs
    current_bright <- read <$> readFile path
    let new_bright = show $ calc current_bright args
    putStrLn new_bright
    writeFile path new_bright
-}


calc :: Int -> [String] -> Int
calc _ (x:_) 
    | Just val <- readMaybe x = if val > max_brightness
        then max_brightness
        else val
calc val ("-d":_) = darken val
calc val ("-b":_) = brighten val
calc val _ = brighten val -- default is brighten

brighten :: Int -> Int
brighten !val = if val > max_brightness - increment 
    then max_brightness
    else val + increment

darken :: Int -> Int
darken !val = if val < increment
    then 0
    else val - increment
