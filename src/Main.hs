module Main (main) where

import App (AlgoState, Options (..), Output, advanceAlgorithms, formatLine, initialAlgoStates, parseArgs)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, isEOF, stderr)
import Text.Read (readMaybe)
import Types (Point (..))

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left err -> do
            hPutStrLn stderr err
            printUsage
            exitFailure
        Right opts -> runProgram opts

runProgram :: Options -> IO ()
runProgram opts = processInteractive [] (initialAlgoStates (optAlgorithms opts))
  where
    step = optStep opts

    processInteractive :: [Point] -> [AlgoState] -> IO ()
    processInteractive accumulated states = do
        eof <- isEOF
        if eof
            then return ()
            else do
                line <- getLine
                case parsePoint line of
                    Left err -> do
                        hPutStrLn stderr err
                        exitFailure
                    Right Nothing -> processInteractive accumulated states
                    Right (Just point) -> do
                        if not (ordered accumulated point)
                            then do
                                hPutStrLn stderr "Входные данные должны быть отсортированы по возрастанию x"
                                exitFailure
                            else do
                                putStrLn $ "< " ++ show (px point) ++ " " ++ show (py point)
                                let newAccumulated = accumulated ++ [point]
                                let (newStates, outputs) = advanceAlgorithms step newAccumulated states
                                mapM_ printOutput outputs
                                processInteractive newAccumulated newStates

    ordered :: [Point] -> Point -> Bool
    ordered [] _ = True
    ordered xs p = px p > px (last xs)

    printOutput :: Output -> IO ()
    printOutput (alg, x, y) = putStrLn $ "> " ++ formatLine alg x y

parsePoint :: String -> Either String (Maybe Point)
parsePoint raw =
    let cleaned = map replaceDelimiter raw
        tokens = words cleaned
     in case tokens of
            [] -> Right Nothing
            [sx, sy] ->
                case (readMaybe sx, readMaybe sy) of
                    (Just x, Just y) -> Right (Just (Point x y))
                    _ -> Left ("Невозможно прочитать точку: " ++ raw)
            _ -> Left ("Некорректный формат строки: " ++ raw)
  where
    replaceDelimiter ';' = ' '
    replaceDelimiter other = other

printUsage :: IO ()
printUsage = do
    putStrLn "Использование: cabal run fp-lab3 -- [--linear] [--newton N] [--step S]"
    putStrLn "  --linear        линейная интерполяция"
    putStrLn "  --newton N      интерполяция Ньютона по N точкам (N >= 2)"
    putStrLn "  --step S        шаг по оси X (положительное число, по умолчанию 1.0)"
    putStrLn "  --help          завершение программы с выводом помощи по использованию"
