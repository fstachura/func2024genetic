module Random (
    randFromState,
    randListFromState,
    randPointsFromState,
    randSelectFromState
) where

import Control.Monad.State
import System.Random

randFromState :: (Random a) => (a, a) -> State StdGen a
randFromState range = do
    gen <- get
    let (result, newgen) = randomR range gen
    put newgen
    return $ result

randListFromState :: (Random a) => (a, a) -> Int -> State StdGen [a]
randListFromState range n = mapM (const $ randFromState range) [1..n]

randPointsFromState :: (Random a) => ((a, a), (a, a)) -> Int -> State StdGen [(a, a)]
randPointsFromState (a, b) n = do
    ra <- randListFromState a n
    rb <- randListFromState b n
    return $ zip ra rb

-- TODO slow
randSelectFromState :: [a] -> State StdGen a
randSelectFromState l = do
    i <- randFromState (0, (length l)-1)
    return $ (l !! i)

