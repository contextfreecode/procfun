import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.State
import System.Clock
import System.Random
import System.Random.Stateful

newtype Debug = Debug {
    errCount :: Integer
}

debugDefault = Debug {
    errCount = 0
}

data Game = Game {
    answer :: Integer,
    done :: Bool,
    guesses :: Integer,
    high :: Integer
}

gameAnswer = answer
gameHigh = high

gameDefault = Game {
    answer = undefined,
    done = False,
    guesses = 0,
    high = undefined
}

askGuess :: Integer -> IO Integer
askGuess high = do
    putStr $ "Guess a number between 1 and " ++ show high ++ ": "
    text <- getLine
    evaluate $ read text

askGuessMulti :: Integer -> StateT Debug IO Integer
askGuessMulti high =
    -- Use explicit ErrorCall to avoid catching Ctrl+C
    lift (askGuess high) `catchStateT` \(ErrorCall err) -> do
        lift $ putStrLn "I didn't understand"
        debug <- get
        put $ debug { errCount = errCount debug + 1 }
        askGuessMulti high

-- From https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/exceptions-and-monad-transformers
catchStateT ::
    Exception e => StateT s IO a -> (e -> StateT s IO a) -> StateT s IO a
catchStateT action handler = do
    state <- get
    (result, state') <- liftIO $ runStateT action state `catch` \err ->
        runStateT (handler err) state
    put state'
    return result

pickAnswer :: Integer -> IO Integer
pickAnswer high =
    randomRIO (1, high)

pickAnswer2 :: RandomGen gen => Integer -> gen -> (Integer, gen)
pickAnswer2 high = uniformR (1, high)

pickAnswer3 :: StatefulGen gen m => Integer -> gen -> m Integer
pickAnswer3 high = uniformRM (1, high)

play :: Game -> StateT Debug IO Game
play game = do
    guess <- askGuessMulti $ high game
    return game

timeSeed :: IO Integer
timeSeed = do
    time <- getTime Monotonic
    return $ toNanoSecs time

main :: IO ()
main = do
    answer <- pickAnswer high
    let game = gameDefault { answer = answer, high = high }
    (result, debug) <- runStateT (play game) debugDefault
    print answer
    print $ errCount debug
    -- seed <- timeSeed
    -- let g3 = mkStdGen seed
    -- let (v5, g4) = pickAnswer2 100 g3
    -- -- See https://hackage.haskell.org/package/random-1.2.1/docs/src/System.Random.html#getStdRandom
    -- v2 <- getStdRandom $ pickAnswer2 100
    -- print answer
    -- print val
    -- print v2
    -- print v3
    -- -- v4 <- pickAnswer3 100 (newIOGenM gen)
    -- -- print v4
    -- print v5
    where
        high = 100
        gen = mkStdGen 5
        (v3, g2) = pickAnswer2 100 gen
        -- v4 = do
        --     (return $ pickAnswer2 100 g2) :: StatefulGen Integer
        val = uniformR (1, 100 :: Integer) (mkStdGen 5)
