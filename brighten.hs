-- this program would prabably be better with IO.Strict.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
import System.Environment (getArgs)

path :: FilePath
-- path = "/sys/class/backlight/intel_backlight/brightness"
path = "/tmp/HSTEST"

max_brightness, increment :: Int
max_brightness = 7812
increment = 500

main :: IO ()
main = do
    current_bright <- readFile path
    let new_bright = show $ brighten (read current_bright)
    putStrLn new_bright
    writeFile path new_bright
{- -- alternate implementation, only opens the file once but is uglier...
main = do
    handle <- openFile path ReadWriteMode
    current_bright <- read <$> hGetLine handle
    let new_bright = darken current_bright
    print new_bright
    hSeek handle AbsoluteSeek 0
    hPutStrLn handle (show new_bright)
    hSetFileSize handle (fromIntegral (length (show new_bright) + 1))
    hClose handle
-}

brighten :: Int -> Int
brighten !val = if val > max_brightness - increment 
    then max_brightness
    else val + increment

darken :: Int -> Int
darken !val = if val < increment
    then 0
    else val - increment
