-- Test with ST monads

import Control.Monad
import Control.Monad.ST
import Data.Array.ST

-- Helper: similar to a fold but function decides how many pieces of the array it consumes
collect f v [] = v
collect f v a = collect f v' a'
    where (v', a') = f v a

splitLines str w = collect func [] str
    where
        func lines rest = (lines ++ [line], rest')
            where (line, rest') = splitAt w rest

-- fill an array
makeArray w h v = runST $ do
    arr <- newArray (0, w*h-1) v :: ST s (STArray s Int Char)
    writeArray arr w '1'
    writeArray arr (w*2-1) '2'
    getElems arr

main = do
    putStrLn . unlines $ splitLines (makeArray 4 4 '-') 4
