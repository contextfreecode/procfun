import Control.Monad.IO.Class
import System.Clock
import System.Random
import System.Random.Stateful

pickAnswer :: Int -> IO Int
pickAnswer high =
    randomRIO (1, high)

pickAnswer2 :: RandomGen gen => Int -> gen -> (Int, gen)
pickAnswer2 high = uniformR (1, high)

pickAnswer3 :: StatefulGen gen m => Int -> gen -> m Int
pickAnswer3 high = uniformRM (1, high)

timeSeed :: IO Integer
timeSeed = do
    time <- getTime Monotonic
    return $ toNanoSecs time

main :: IO ()
main = do
    seed <- timeSeed
    let g3 = mkStdGen $ fromIntegral seed
    let (v5, g4) = pickAnswer2 100 g3
    answer <- pickAnswer high
    v2 <- getStdRandom $ pickAnswer2 100
    print answer
    print val
    print v2
    print v3
    -- v4 <- pickAnswer3 100 (newIOGenM gen)
    -- print v4
    print v5
    where
        high = 100
        gen = mkStdGen 5
        (v3, g2) = pickAnswer2 100 gen
        -- v4 = do
        --     (return $ pickAnswer2 100 g2) :: StatefulGen Int
        val = uniformR (1, 100 :: Int) (mkStdGen 5)
