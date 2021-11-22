import Control.Monad.IO.Class
import System.Clock
import System.Random
import System.Random.Stateful

pickAnswer :: Int -> IO Int
pickAnswer high =
    randomRIO (1, high)

pickAnswer2 :: RandomGen gen => Int -> gen -> (Int, gen)
pickAnswer2 high = uniformR (1, high :: Int)

main :: IO ()
main = do
    answer <- pickAnswer high
    v2 <- getStdRandom $ pickAnswer2 100
    print answer
    print val
    print v2
    print v3
    where
        high = 100
        gen = mkStdGen 5
        v3 = pickAnswer2 100 gen
        val = uniformR (1, 100 :: Int) (mkStdGen 5)
