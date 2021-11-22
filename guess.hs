import Control.Monad.IO.Class
import System.Clock
import System.Random
import System.Random.Stateful

pickAnswer :: Int -> IO Int
pickAnswer high =
    randomRIO (1, high)

-- p2 :: StatefulGen g m => Int g -> m Int
-- p2 :: (Random a, Control.Monad.IO.Class.MonadIO m, Num a) => a -> p -> m a
p2 high =
    uniformR (1, 10 :: Int)

main :: IO ()
main = do
    answer <- pickAnswer high
    v2 <- getStdRandom $ p2 10
    print answer
    print val
    print v2
    print v3
    where
        high = 100
        rng = mkStdGen 5
        v3 = p2 10 rng
        val = uniformR (1, 10 :: Int) (mkStdGen 5)
