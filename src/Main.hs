import Control.Monad.State
import qualified Data.List
import System.Random
import Data.Bits
import TestFunctions
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
    st <- get
    let (result, newgen) = runState m (generator st)
    modify (\nst -> nst { generator = newgen })
    return $ result

randomCombineInt :: (Bits n, Random n, Num n, Integral n) => Int -> n -> n -> State (GeneticState a) n
randomCombineInt bits a b = do
    mask <- runInRand (randFromState (0, complement 0))
    return $ (.|.) ((.&.) a mask) ((.&.) b (complement mask))

randomCrossoverInt :: (Bits n, Integral n) => Int -> n -> n -> State (GeneticState a) n
randomCrossoverInt bits na nb = do
    let range = (0, bits) :: (Int, Int)
    crossover <- runInRand $ randFromState range
    let mask = shiftL (complement 0) crossover
    let result = (.|.) ((.&.) na mask) ((.&.) nb (complement mask))
    return $ result

-- intermediate reocombination: a1*b+a2*(1-b) where b in [-d,1+d]

intermediateRecombinationMate :: (Random a, RealFloat a) => a -> (Gene a) -> (Gene a) -> State (GeneticState a) (Gene a)
intermediateRecombinationMate d (ax, ay) (bx, by) = do
    dx <- runInRand $ randFromState (-d, 1+d)
    dy <- runInRand $ randFromState (-d, 1+d)

    return $ (ax*dx + bx*(1-dx), ay*dy + by*(1-dy))

-- one point crossover: pick x bits from gene a, rest from gene b
-- float is converted to (mantisa, exponent), there is 50% chance that exponent will be randomly selected from either gene
-- instead of being a combination of both exponents
-- for each mantisa/exponent sign is randomly propagated from one of the genes

onePointCrossoverMate :: (RealFloat a) => (Gene a) -> (Gene a) -> State (GeneticState a) (Gene a)
onePointCrossoverMate a b = do
    let (m_ax, e_ax) = decodeFloat (fst a)
    let (m_ay, e_ay) = decodeFloat (snd a)
    let (m_bx, e_bx) = decodeFloat (fst b)
    let (m_by, e_by) = decodeFloat (snd b)

    new_mx <- (randomCrossoverInt 52 m_ax m_bx)
    new_ex <- (randomCrossoverInt 11 e_ax e_bx)
    new_my <- (randomCrossoverInt 52 m_ay m_by)
    new_ey <- (randomCrossoverInt 11 e_ay e_by)

    let newx = encodeFloat new_mx new_ex
    let newy = encodeFloat new_my new_ey

    return (newx, newy)

-- pick each bit randomly from one of the genes
-- float is converted to (mantisa, exponent), operation is repeated for both mantisa and exponent
-- mantisa/exponent sign is randomly propagated from one of the genes

uniformBinaryCrossoverMate :: (RealFloat a) => (Gene a) -> (Gene a) -> State (GeneticState a) (Gene a)
uniformBinaryCrossoverMate a b = do
    let (m_ax, e_ax) = decodeFloat (fst a)
    let (m_ay, e_ay) = decodeFloat (snd a)
    let (m_bx, e_bx) = decodeFloat (fst b)
    let (m_by, e_by) = decodeFloat (snd b)

    new_mx <- randomCombineInt 52 m_ax m_bx
    new_ex <- randomCombineInt 11 e_ax e_bx
    new_my <- randomCombineInt 52 m_ay m_by
    new_ey <- randomCombineInt 11 e_ay e_by

    let newx = encodeFloat new_mx new_ex
    let newy = encodeFloat new_my new_ey

    return (newx, newy)

noMutation :: (Gene a) -> State (GeneticState a) (Gene a)
noMutation a = return a

uniformMutation :: (Random a, RealFloat a) => (Gene a) -> State (GeneticState a) (Gene a)
uniformMutation (x, y) = do
    s <- get
    let ((ax, bx), (ay, by)) = (functionArea s)
    let cx = (bx - ax) / 10
    let cy = (by - ay) / 10
    dx <- runInRand (randFromState (-cx, cx))
    dy <- runInRand (randFromState (-cy, cy))
    return $ (x + dx, y + dy)

randomBitFlipMutation :: (RealFloat a) => (Gene a) -> State (GeneticState a) (Gene a)
randomBitFlipMutation (x, y) = do
    let (mx, ex) = decodeFloat x
    let (my, ey) = decodeFloat y

    let bit_size_m = case (bitSizeMaybe mx) of
            Just n -> n
            Nothing -> 52
    let bit_size_e = (bitSize ex)

    bit_to_flip <- runInRand $ randFromState (0, 2*(bit_size_m+bit_size_e))

    let flipBit b n = xor n (setBit 0 b)

    let ((new_mx, new_ex), (new_my, new_ey)) =
                if bit_to_flip < bit_size_m then
                    ((flipBit bit_to_flip mx, ex), (my, ey))
                else if bit_to_flip < bit_size_m+bit_size_e then
                    ((mx, flipBit (bit_to_flip-bit_size_m) ex), (my, ey))
                else if bit_to_flip < 2*bit_size_m+bit_size_e then
                    ((mx, ex), (flipBit (bit_to_flip-bit_size_m-bit_size_e) my, ey))
                else
                    ((mx, ex), (my, flipBit (bit_to_flip-2*bit_size_m-bit_size_e) ey))

    let nx = encodeFloat new_mx new_ex
    let ny = encodeFloat new_my new_ey

    return (nx, ny)

repeatMutation :: (a -> State (GeneticState s) b) -> Int -> a -> State (GeneticState s) b
repeatMutation mut n gene
    | n <= 1 = mut gene
    | otherwise = do
        result <- mut gene
        repeatMutation mut (n-1) gene

randomRepeatMutation :: (a -> State (GeneticState s) b) -> (Int, Int) -> a -> State (GeneticState s) b
randomRepeatMutation mut range gene = do
    n <- runInRand $ randFromState range
    repeatMutation mut n gene

runRandomMutation :: [a -> State (GeneticState s) b] -> a -> State (GeneticState s) b
runRandomMutation mutations gene = do
    mut <- runInRand (randSelectFromState mutations)
    mut gene

runRandomMatingStrategy :: [a -> a -> State (GeneticState s) b] -> a -> a -> State (GeneticState s) b
runRandomMatingStrategy strategies a b = do
    s <- runInRand (randSelectFromState strategies)
    s a b

generateNewGene :: [Gene a] -> State (GeneticState a) (Gene a)
generateNewGene fitGenes = do
    st <- get
    geneA <- runInRand (randSelectFromState fitGenes)
    geneB <- runInRand (randSelectFromState fitGenes)
    let mateGenes = matingStrategy st
    geneR <- (mateGenes geneA geneB)
    (mutationStrategy st) geneR

estimateFitness :: (Ord a, Fractional a) => (Gene a -> a) -> Gene a -> State (GeneticState a) a
estimateFitness f p = do
    st <- get
    let ((ax, bx), (ay, by)) = functionArea st
    let (x, y) = p
    return $ if x < ax || x > bx || y < ay || y > by then
                1 / 0
             else
                f p

rankByFitness :: (RealFloat a) => (Gene a -> a) -> [Gene a] -> State (GeneticState a) [(Gene a, a)]
rankByFitness f genesToRank = do
    fitList <- mapM (\gene -> do
            fitness <- estimateFitness f gene
            return (gene, fitness))
            genesToRank
    return $ Data.List.sortBy (\(_, fa) (_, fb) -> compare fa fb) fitList

iterateGenetic :: (RealFloat a, Random a) => State (GeneticState a) (Gene a, a)
iterateGenetic = do
    st <- get
    if (iterations st) == 0 then do
        let gene = head (genes st)
        return $ (gene, (function st) gene)
    else do
        let numOfGenesToCreate = (length (genes st))-(numOfFitGenes st)
        rankedByFitness <- rankByFitness (function st) (genes st)
        let fitResults = take (numOfFitGenes st) rankedByFitness
        let fitGenes = (map fst fitResults)
        newGenes <- mapM (\_ -> generateNewGene fitGenes) [1..numOfGenesToCreate]
        modify (\nst -> nst { genes = fitGenes ++ newGenes, iterations = ((iterations st)-1) })
        iterateGenetic

randomGeneticState :: IO (GeneticState Double)
randomGeneticState = do
    gen <- getStdGen

    let ((_, func, fa, _), newgen) = runState (randSelectFromState testFunctions) gen

    let numberOfGenes = 1000

    let (random_genes, newgen2) = runState (randPointsFromState fa numberOfGenes) newgen

    return $ GeneticState {
        function = func,
        functionArea = fa,
        generator = newgen2,
        matingStrategy = (intermediateRecombinationMate 0.25),
        mutationStrategy = randomBitFlipMutation,
        genes = random_genes,
        numOfFitGenes = 100,
        iterations = 1000
    }


main :: IO ()
main = do
    _ <- mapM (\(name, func, fa, expectedResult) -> do
            let numberOfGenes = 1000
            --let functionArea :: ((Double, Double), (Double, Double)) = fa

            gen <- getStdGen
            let (random_genes, newgen) = runState (randPointsFromState fa numberOfGenes) gen

            let gst = GeneticState {
                function = func,
                functionArea = fa,
                generator = newgen,
                matingStrategy = runRandomMatingStrategy [
                    (intermediateRecombinationMate 2),
                    onePointCrossoverMate,
                    uniformBinaryCrossoverMate
                ],
                mutationStrategy = runRandomMutation [
                    uniformMutation,
                    randomRepeatMutation randomBitFlipMutation (1,64)
                ],
                genes = random_genes,
                numOfFitGenes = 100,
                iterations = 100
            }

            let (result, newgst) = runState (iterateGenetic) gst
            let eresult = func expectedResult

            setStdGen (generator newgst)

            putStrLn $ name
            putStrLn $ show $ result
            putStrLn $ "f" ++ (show expectedResult) ++ " = " ++ (show eresult) ++
                        " (d = " ++ (show $ abs $ (snd result)-eresult) ++ ")"
        ) testFunctions --(filter (\(n, _, _, _) -> n `elem` ["eggholder", "bohachevsky"]) testFunctions)

    return $ ()

-- dropwave
-- eggholder

