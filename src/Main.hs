import Control.Monad.State
import Data.List
import System.Random
import Debug.Trace
import Data.Bits
import TestFunctions
import Utils
import Random

type Gene a = (a, a)

data GeneticState a = GeneticState {
    function :: (Gene a) -> a,
    functionArea :: ((a, a), (a, a)),
    matingStrategy :: (Gene a) -> (Gene a) -> State (GeneticState a) (Gene a),
    mutationStrategy :: (Gene a) -> State (GeneticState a) (Gene a),
    generator :: StdGen,
    genes :: [Gene a], 
    numOfFitGenes :: Int,
    iterations :: Int
}

runInRand :: State StdGen a -> State (GeneticState b) a
runInRand m = do
    state <- get
    let (result, newgen) = runState m (generator state)
    modify (\state -> state { generator = newgen })
    return $ result

randomCombineInt :: (Bits n, Show n, Integral n) => Int -> n -> n -> State (GeneticState a) (n, [Bool])
randomCombineInt bits na nb = do
    let range = (0, 1) :: (Int, Int)
    probs <- runInRand (randListFromState range bits)
    signProb <- runInRand (randFromState range)
    let conva = (numToBits bits (abs na))
    let convb = (numToBits bits (abs nb))
    let result = map 
            (\(p, a, b) -> if p == 0 then a else b) 
            (zip3 probs conva convb)
    return $ ((if signProb == 0 then (sign na) else (sign nb)), result)

randomCrossoverInt :: (Bits n, Show n, Integral n) => Int -> n -> n -> State (GeneticState a) (n, [Bool])
randomCrossoverInt bits na nb = do
    let range = (0, bits) :: (Int, Int)
    crossover <- runInRand $ randFromState range
    let conva = (numToBits bits (abs na))
    let convb = (numToBits bits (abs nb))
    let result = (take crossover conva) ++ (drop crossover convb)
    return $ ((sign na), result)

-- intermediate reocombination: a1*b+a2*(1-b) where b in [-d,1+d]

intermediateRecombinationMate :: (Show a, Random a, RealFloat a) => a -> (Gene a) -> (Gene a) -> State (GeneticState a) (Gene a)
intermediateRecombinationMate d (ax, ay) (bx, by) = do
    bx <- runInRand $ randFromState (-d, 1+d)
    by <- runInRand $ randFromState (-d, 1+d)

    return $ (ax*bx + bx*(1-bx), ay*by + by*(1-by))

-- one point crossover: pick x bits from gene a, rest from gene b
-- float is converted to (mantisa, exponent), there is 50% chance that exponent will be randomly selected from either gene
-- instead of being combined (like mantisa)
-- for each mantisa/exponent sign is randomly propagated from one of the genes

onePointCrossoverMate :: (Show a, RealFloat a) => (Gene a) -> (Gene a) -> State (GeneticState a) (Gene a)
onePointCrossoverMate a b = do
    let (max, eax) = decodeFloat (fst a)
    let (may, eay) = decodeFloat (snd a)
    let (mbx, ebx) = decodeFloat (fst b)
    let (mby, eby) = decodeFloat (snd b)

    eyprob <- runInRand (randFromState (0 :: Int, 1))

    (snewmx, newmx) <- randomCrossoverInt 64 max mbx 

    let crossoverOrPick = \a b -> do
            exprob <- runInRand (randFromState (0 :: Int, 1))
            if exprob == 1 then do
                (sign, result) <- randomCrossoverInt 11 a b
                return $ sign*(bitsToNum result)
            else do
                newex <- runInRand (randSelectFromState [a, b])
                return $ newex

    (snewmy, newmy) <- randomCrossoverInt 64 may mby 

    newex <- crossoverOrPick eax ebx
    newey <- crossoverOrPick eay eby

    let newx = encodeFloat (snewmx*(bitsToNum newmx)) newex
    let newy = encodeFloat (snewmy*(bitsToNum newmy)) newey

    return (newx, newy)

-- pick each bit randomly from one of the genes
-- float is converted to (mantisa, exponent), operation is repeated for both mantisa and exponent
-- mantisa/exponent sign is randomly propagated from one of the genes

uniformBinaryCrossoverMate :: (Show a, RealFloat a) => (Gene a) -> (Gene a) -> State (GeneticState a) (Gene a)
uniformBinaryCrossoverMate a b = do
    let (max, eax) = decodeFloat (fst a)
    let (may, eay) = decodeFloat (snd a)
    let (mbx, ebx) = decodeFloat (fst b)
    let (mby, eby) = decodeFloat (snd b)

    (snewmx, newmx) <- randomCombineInt 64 max mbx 
    (snewex, newex) <- randomCombineInt 11 eax ebx 
    (snewmy, newmy) <- randomCombineInt 64 may mby 
    (snewey, newey) <- randomCombineInt 11 eay eby 

    let newx = encodeFloat (snewmx*(bitsToNum newmx)) (snewex*(bitsToNum newex))
    let newy = encodeFloat (snewmy*(bitsToNum newmy)) (snewey*(bitsToNum newey))

    return (newx, newy)
    
noMutation :: (Random a, Ord a, RealFloat a) => (Gene a) -> State (GeneticState a) (Gene a)
noMutation a = return a

uniformMutation :: (Random a, Ord a, RealFloat a) => (Gene a) -> State (GeneticState a) (Gene a)
uniformMutation (x, y) = do
    s <- get
    let ((ax, bx), (ay, by)) = (functionArea s)
    let cx = (bx - ax) / 10
    let cy = (by - ay) / 10
    dx <- runInRand (randFromState (-cx, cx))
    dy <- runInRand (randFromState (-cy, cy))
    return $ (x + dx, y + dy)

randomBitFlipMutation :: (Random a, Ord a, RealFloat a) => (Gene a) -> State (GeneticState a) (Gene a)
randomBitFlipMutation (x, y) = do 
    let (mx, ex) = decodeFloat x
    let (my, ey) = decodeFloat y

    let sgmx = sign mx
    let sgmy = sign my
    let sgex = sign ex
    let sgey = sign ey
    let bmx = numToBits 64 mx
    let bmy = numToBits 64 my
    let bex = numToBits 11 ex
    let bey = numToBits 11 ey

    fmx <- runInRand $ randFromState (0, (length bmx)-1)
    fmy <- runInRand $ randFromState (0, (length bmy)-1)
    fex <- runInRand $ randFromState (0, 2*((length bex)-1))
    fey <- runInRand $ randFromState (0, 2*((length bey)-1))

    let nx = encodeFloat (sgmx*(bitsToNum $ flipBit fmx bmx)) (sgex*(bitsToNum $ flipBit fex bex))
    let ny = encodeFloat (sgmy*(bitsToNum $ flipBit fmy bmy)) (sgey*(bitsToNum $ flipBit fey bey))

    return (nx, ny)

runRandomMutation :: [a -> State (GeneticState s) b] -> a -> State (GeneticState s) b
runRandomMutation mutations gene = do
    mut <- runInRand (randSelectFromState mutations) 
    mut gene

runRandomMatingStrategy :: [a -> a -> State (GeneticState s) b] -> a -> a -> State (GeneticState s) b
runRandomMatingStrategy strategies a b = do
    s <- runInRand (randSelectFromState strategies) 
    s a b

generateNewGene :: (Show a, Random a, Ord a, RealFloat a) => [Gene a] -> State (GeneticState a) (Gene a)
generateNewGene fitGenes = do
    state <- get
    geneA <- runInRand (randSelectFromState fitGenes)
    geneB <- runInRand (randSelectFromState fitGenes)
    ---- TODO check if genes are not the same?
    let mateGenes = matingStrategy state
    geneR <- (mateGenes geneA geneB)
    (mutationStrategy state) geneR

estimateFitness :: (Show a, Ord a, Num a, Fractional a) => (Gene a -> a) -> Gene a -> State (GeneticState a) a
estimateFitness f p = do
    state <- get
    let ((ax, bx), (ay, by)) = functionArea state
    let (x, y) = p
    return $ if x < ax || x > bx || y < ay || y > by then
                1 / 0
             else
                f p

rankByFitness :: (Show a, RealFloat a) => (Gene a -> a) -> [Gene a] -> State (GeneticState a) [(Gene a, a)]
rankByFitness f genes = do
    fitList <- mapM (\gene -> do 
            fitness <- estimateFitness f gene
            return (gene, fitness)) 
            genes
    return $ sortBy (\(_, fa) (_, fb) -> compare fa fb) fitList

iterateGenetic :: (Show a, RealFloat a, Random a) => State (GeneticState a) (Gene a, a)
iterateGenetic = do
    state <- get
    if (iterations state) == 0 then do
        let gene = head (genes state)
        return $ (gene, (function state) gene)
    else do
        let numOfGenesToCreate = (length (genes state))-(numOfFitGenes state)
        rankedByFitness <- rankByFitness (function state) (genes state)
        let fitResults = take (numOfFitGenes state) rankedByFitness
        let fitGenes = (map fst fitResults)
        newGenes <- mapM (\_ -> generateNewGene fitGenes) [1..numOfGenesToCreate]
        modify (\state -> state { genes = fitGenes ++ newGenes, iterations = ((iterations state)-1) })
        iterateGenetic

randomGeneticState :: IO (GeneticState Double)
randomGeneticState = do
    gen <- getStdGen

    let ((name, function, fa, expectedResult), newgen) = runState (randSelectFromState testFunctions) gen

    let numberOfGenes = 1000
    let functionArea = fa

    let (genes, newgen) = runState (randPointsFromState functionArea numberOfGenes) gen

    return $ GeneticState {
        function = function,
        functionArea = functionArea,
        generator = newgen,
        matingStrategy = (intermediateRecombinationMate 0.25),
        mutationStrategy = randomBitFlipMutation,
        genes = genes,
        numOfFitGenes = 100,
        iterations = 1000
    }


main :: IO ()
main = do 
    _ <- mapM (\(name, function, fa, expectedResult) -> do 
            let numberOfGenes = 1000
            --let functionArea :: ((Double, Double), (Double, Double)) = fa
            let functionArea = fa

            gen <- getStdGen
            let (genes, newgen) = runState (randPointsFromState functionArea numberOfGenes) gen

            let gstate = GeneticState {
                function = function,
                functionArea = functionArea,
                generator = newgen,
                matingStrategy = runRandomMatingStrategy [
                    (intermediateRecombinationMate 0.25)
                    --onePointCrossoverMate, 
                    --uniformBinaryCrossoverMate
                ],
                mutationStrategy = runRandomMutation [
                    uniformMutation,
                    randomBitFlipMutation
                ],
                genes = genes,
                numOfFitGenes = 100,
                iterations = 100
            }

            let (result, newgstate) = runState (iterateGenetic) gstate
            let eresult = function expectedResult

            setStdGen (generator newgstate)

            putStrLn $ name
            putStrLn $ show $ result
            putStrLn $ "f" ++ (show expectedResult) ++ " = " ++ (show eresult) ++ 
                        " (d = " ++ (show $ abs $ (snd result)-eresult) ++ ")"
        ) (filter (\(n, _, _, _) -> n `elem` ["eggholder", "dropwave"]) testFunctions)

    return $ ()


-- dropwave
-- eggholder

