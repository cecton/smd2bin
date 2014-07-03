#!/usr/bin/env runhaskell

import System.IO
import System.Environment
import qualified Data.ByteString as BS

-- | main function
main :: IO ()
main = do
    cmd  <- getProgName
    args <- getArgs
    case args of
        [] -> putStrLn $ "usage: " ++ cmd ++ " <file.smd>"
        [smd, bin]  -> do
            content <- BS.readFile smd
            BS.writeFile bin (convert_smd_bin content 512 16384)

smd_bin :: BS.ByteString -> Int -> Int -> BS.ByteString
smd_bin buf block_size block_split
    | BS.null buf = BS.empty
    | otherwise   = BS.pack
        (concat [ [j, i]
                | (i, j) <- (\(x, y) -> BS.zip x y)
                            (BS.splitAt block_split buf)])
        `BS.append` (smd_bin (BS.drop block_size buf) block_size block_split)

convert_smd_bin :: BS.ByteString -> Int -> Int -> BS.ByteString
convert_smd_bin buf header_size block_size
    = smd_bin (BS.drop header_size buf) block_size (div block_size 2)
