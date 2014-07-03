#!/usr/bin/env runhaskell

import System.IO
import System.Environment
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L

-- | main function
main :: IO ()
main = do
    cmd  <- getProgName
    args <- getArgs
    case args of
        [] -> putStrLn $ "usage: " ++ cmd ++ " <infile.smd> [<outfile.bin>]"
        [smd]  -> do
            content <- BS.readFile smd
            L.putStr (convert_smd_bin content)
        [smd, bin]  -> do
            content <- BS.readFile smd
            L.writeFile bin (convert_smd_bin content)

interleaveBlocks :: [(BS.ByteString, BS.ByteString)] -> [BS.ByteString]
interleaveBlocks []          =  []
interleaveBlocks ((x, y):xs) =
    BS.concat (BS.zipWith (\ a b -> BS.pack [b, a]) x y)
    : interleaveBlocks xs

toBlocks :: BS.ByteString -> [(BS.ByteString, BS.ByteString)]
toBlocks buf
    | BS.null buf =  []
    | otherwise   = let (before, after) = BS.splitAt 16384 buf
                    in  BS.splitAt 8192 before : toBlocks after

smd_bin = L.fromChunks . interleaveBlocks . toBlocks

convert_smd_bin :: BS.ByteString -> L.ByteString
convert_smd_bin = smd_bin . BS.drop 512
