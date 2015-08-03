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
increment = 350

main :: IO ()
main = do
    args <- getArgs
    handle <- openFile path ReadWriteMode
    current_bright <- read <$> hGetLine handle
    let new_bright = show $ calc current_bright args
--     putStrLn new_bright
    hSeek handle AbsoluteSeek 0
    hPutStrLn handle (new_bright)
    hSetFileSize handle (fromIntegral (length (new_bright) + 1))
    hClose handle

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
darken !val = if val <= increment
    then 1
    else val - increment
