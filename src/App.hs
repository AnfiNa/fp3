module App (
    Options (..),
    AlgoState,
    Output,
    parseArgs,
    initialAlgoStates,
    advanceAlgorithms,
    formatLine,
    simulateSequential,
)
where

import Data.List (unfoldr)
import Interpolation
import Text.Read (readMaybe)
import Types

data Options = Options
    { optStep :: Double
    , optAlgorithms :: [Algorithm]
    }
    deriving (Show)

data AlgoState = AlgoState Algorithm (Maybe Double)
    deriving (Eq, Show)

type Output = (Algorithm, Double, Double)

parseArgs :: [String] -> Either String Options
parseArgs args =
    case walk args defaultOptions of
        Left err -> Left err
        Right opts -> checkResult opts
  where
    defaultOptions = Options{optStep = 1.0, optAlgorithms = []}

    walk :: [String] -> Options -> Either String Options
    walk [] opts = Right opts
    walk ("--linear" : rest) opts =
        walk rest (addAlgorithm AlgLinear opts)
    walk ("--newton" : value : rest) opts =
        case readMaybe value of
            Just n | n >= 2 -> walk rest (addAlgorithm (AlgNewton n) opts)
            _ -> Left "Флаг --newton требует целое число >= 2"
    walk ["--newton"] _ = Left "Флаг --newton требует параметр"
    walk ("--step" : value : rest) opts =
        case readMaybe value of
            Just s | s > 0 -> walk rest (opts{optStep = s})
            _ -> Left "Флаг --step требует положительное число"
    walk ["--step"] _ = Left "Флаг --step требует параметр"
    walk ("-h" : _) _ = Left ""
    walk ("--help" : _) _ = Left ""
    walk (unknown : _) _ = Left ("Неизвестный аргумент: " ++ unknown)

    addAlgorithm :: Algorithm -> Options -> Options
    addAlgorithm alg opts
        | alg `elem` optAlgorithms opts = opts
        | otherwise = opts{optAlgorithms = optAlgorithms opts ++ [alg]}

    checkResult :: Options -> Either String Options
    checkResult opts
        | null (optAlgorithms opts) = Left "Нужно указать хотя бы один алгоритм (например, --linear)"
        | optStep opts <= 0 = Left "Шаг дискретизации должен быть положительным"
        | otherwise = Right opts

initialAlgoStates :: [Algorithm] -> [AlgoState]
initialAlgoStates algorithms = [AlgoState alg Nothing | alg <- algorithms]

advanceAlgorithms :: Double -> [Point] -> [AlgoState] -> ([AlgoState], [Output])
advanceAlgorithms step pts states =
    let results = map (advanceOne step pts) states
        newStates = map fst results
        allOutputs = concatMap snd results
     in (newStates, allOutputs)

advanceOne :: Double -> [Point] -> AlgoState -> (AlgoState, [Output])
advanceOne _ [] st = (st, [])
advanceOne step pts@(first : _) (AlgoState kind nextX) =
    let startX = case nextX of
            Nothing -> px first
            Just x -> x
        lastX = px (last pts)
        generateNext current
            | current > lastX = Nothing
            | otherwise =
                case calculate kind pts current of
                    Nothing -> Nothing
                    Just value -> Just ((kind, current, value), current + step)
        outputs = unfoldr generateNext startX
        finalX = if null outputs then startX else (\(_, x, _) -> x) (last outputs) + step
     in (AlgoState kind (Just finalX), outputs)

calculate :: Algorithm -> [Point] -> Double -> Maybe Double
calculate AlgLinear = linearValue
calculate (AlgNewton n) = newtonValue n

formatLine :: Algorithm -> Double -> Double -> String
formatLine alg x y = algorithmName alg ++ ": " ++ show x ++ " " ++ show y

simulateSequential :: Double -> [Algorithm] -> [Point] -> [Output]
simulateSequential step algorithms points =
    let initialStates = initialAlgoStates algorithms
        processPoint (accPoints, states, allOutputs) point =
            let newAccPoints = accPoints ++ [point]
                (newStates, outputs) = advanceAlgorithms step newAccPoints states
             in (newAccPoints, newStates, allOutputs ++ outputs)
        (_, _, finalOutputs) = foldl processPoint ([], initialStates, []) points
     in finalOutputs
