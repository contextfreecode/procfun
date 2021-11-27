import Control.Exception
import Control.Monad.State
import System.Random

newtype Debug = Debug
  { errCount :: Integer
  }

debugDefault =
  Debug
    { errCount = 0
    }

data Game = Game
  { answer :: Integer,
    done :: Bool,
    guesses :: Integer,
    high :: Integer
  }

gameDefault =
  Game
    { answer = undefined,
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
    -- Alternatively use a global IORef for errCount.
    debug <- get
    put $ debug {errCount = errCount debug + 1}
    askGuessMulti high

-- From https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/exceptions-and-monad-transformers
catchStateT ::
  Exception e => StateT s IO a -> (e -> StateT s IO a) -> StateT s IO a
catchStateT action handler = do
  state <- get
  (result, state') <-
    liftIO $
      runStateT action state `catch` \err ->
        runStateT (handler err) state
  put state'
  return result

pickAnswer :: Integer -> IO Integer
pickAnswer high =
  randomRIO (1, high)

pickAnswer2 :: RandomGen gen => Integer -> gen -> (Integer, gen)
pickAnswer2 high = uniformR (1, high)

play :: Game -> StateT Debug IO Game
play game = do
  guess <- askGuessMulti $ high game
  lift $ game `report` guess
  let next = game `update` guess
  if done next then return next else play next

report :: Game -> Integer -> IO ()
report game guess =
  putStrLn $ show guess ++ " is " ++ description
  where
    description = case compare guess (answer game) of
      LT -> "too low"
      GT -> "too high"
      EQ -> "the answer!"

update :: Game -> Integer -> Game
update game guess =
  game {done = guess == answer game, guesses = guesses game + 1}

main :: IO ()
main = do
  answer <- pickAnswer high
  -- let (answer, gen2) = pickAnswer2 high $ mkStdGen 1337
  -- answer <- getStdRandom $ pickAnswer2 high
  let game = gameDefault {answer = answer, high = high}
  (result, debug) <- runStateT (play game) debugDefault
  putStrLn $ "Finished in " ++ show (guesses result) ++ " guesses"
  putStrLn $ "Total input errors: " ++ show (errCount debug)
  where
    high = 100
