{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import App
import Control.Monad (unless, zipWithM_)
import Data.Maybe (isNothing)
import Interpolation
import Test.HUnit
import qualified Test.QuickCheck as QC
import Types (Point (Point, px, py))

main :: IO ()
main = runTestTTAndExit tests

-- | Допустимая погрешность для сравнения чисел с плавающей точкой
epsilon :: Double
epsilon = 1e-4

-- | Сравнение двух чисел с погрешностью
approxEqual :: Double -> Double -> Bool
approxEqual a b = abs (a - b) < epsilon

-- | Проверка, что ожидаемое значение совпадает с полученным (Maybe Double)
assertApprox :: String -> Double -> Maybe Double -> Assertion
assertApprox label expected actual =
    case actual of
        Nothing -> assertFailure $ label ++ ": ожидалось значение, получили Nothing"
        Just value ->
            unless (approxEqual expected value) $
                assertFailure $
                    label ++ ": ожидалось " ++ show expected ++ ", получили " ++ show value

-- | Проверка, что результат равен Nothing
assertNothing :: String -> Maybe Double -> Assertion
assertNothing label actual =
    case actual of
        Nothing -> pure ()
        Just value -> assertFailure $ label ++ ": ожидалось Nothing, получили " ++ show value

-- | Проверка двух пар чисел с погрешностью
assertApproxPair :: String -> (Double, Double) -> (Double, Double) -> Assertion
assertApproxPair label (ex, ey) (ax, ay) = do
    unless (approxEqual ex ax) $
        assertFailure $
            label ++ " по x: ожидалось " ++ show ex ++ ", получили " ++ show ax
    unless (approxEqual ey ay) $
        assertFailure $
            label ++ " по y: ожидалось " ++ show ey ++ ", получили " ++ show ay

-- | Тестовые данные для линейной интерполяции
linearPoints :: [Point]
linearPoints = [Point 0 0, Point 2 2]

-- | Тестовые данные для интерполяции Ньютона
newtonPoints :: [Point]
newtonPoints = [Point 0 0, Point 1 1, Point 2 4, Point 3 9]

-- | Модульные тесты линейной интерполяции
linearUnitTests :: Test
linearUnitTests = TestLabel "linear interpolation" $ TestCase $ do
    assertApprox "точка внутри" 1.0 (linearValue linearPoints 1)
    assertApprox "левая граница" 0.0 (linearValue linearPoints 0)
    assertApprox "правая граница" 2.0 (linearValue linearPoints 2)
    assertNothing "нет сегмента" (linearValue linearPoints (-1))

-- | Модульные тесты интерполяции Ньютона
newtonUnitTests :: Test
newtonUnitTests = TestLabel "newton interpolation" $ TestCase $ do
    assertNothing "недостаточно точек" (newtonValue 5 newtonPoints 1.5)
    assertApprox "квадратичная функция" 2.25 (newtonValue 4 newtonPoints 1.5)
    assertApprox "значение в узле" 9.0 (newtonValue 4 newtonPoints 3)

-- | Обёртка QuickCheck-свойства в тест HUnit
qcPropertyTest :: String -> QC.Property -> Test
qcPropertyTest label prop = TestLabel label $ TestCase $ do
    result <- QC.quickCheckWithResult QC.stdArgs{QC.chatty = False} prop
    unless (QC.isSuccess result) $ assertFailure $ QC.output result

-- | Генератор для тестирования линейной интерполяции на аффинных функциях
genLinearCase :: QC.Gen (Double, Double, Double, Double, Double)
genLinearCase = do
    x1 <- QC.choose (-10, 10)
    dx <- QC.choose (1e-3, 5)
    a <- QC.choose (-5, 5)
    b <- QC.choose (-5, 5)
    t <- QC.choose (0, 1)
    pure (x1, dx, a, b, t)

-- | Свойство: линейная интерполяция точно восстанавливает аффинную функцию
prop_linearInterpolatesLine :: QC.Property
prop_linearInterpolatesLine = QC.forAll genLinearCase $ \(x1, dx, a, b, t) ->
    let x2 = x1 + dx
        x = x1 + t * dx
        y1 = a * x1 + b
        y2 = a * x2 + b
        expected = a * x + b
        points = [Point x1 y1, Point x2 y2]
     in case linearValue points x of
            Nothing -> QC.counterexample "linearValue вернула Nothing" False
            Just actual ->
                QC.counterexample ("ожидалось " ++ show expected ++ ", получили " ++ show actual) $
                    approxEqual expected actual

-- | Свойство: вне интервала линейная интерполяция возвращает Nothing
prop_linearOutside :: QC.Property
prop_linearOutside = QC.forAll genLinearCase $ \(x1, dx, a, b, _) ->
    let x2 = x1 + dx
        y1 = a * x1 + b
        y2 = a * x2 + b
        points = [Point x1 y1, Point x2 y2]
        outsideLeft = x1 - dx
        outsideRight = x2 + dx
     in QC.conjoin
            [ QC.counterexample "значение слева" (isNothing (linearValue points outsideLeft))
            , QC.counterexample "значение справа" (isNothing (linearValue points outsideRight))
            ]

-- | Генератор для тестирования интерполяции Ньютона на многочленах
genNewtonCase :: QC.Gen (Int, [Point], Double, [Double])
genNewtonCase = do
    n <- QC.chooseInt (2, 5)
    x0 <- QC.choose (-10, 10)
    deltas <- QC.vectorOf (n - 1) (QC.choose (1e-3, 3))
    let xs = scanl (+) x0 deltas
    coeffs <- QC.vectorOf n (QC.choose (-3, 3))
    let poly x = sum [c * x ^ i | (c, i) <- zip coeffs [0 ..]]
        points = [Point x (poly x) | x <- xs]
    xTest <- QC.choose (-10, 10)
    pure (n, points, xTest, coeffs)

-- | Свойство: интерполяция Ньютона степени n точно воспроизводит многочлен степени ≤ n-1
prop_newtonMatchesPolynomial :: QC.Property
prop_newtonMatchesPolynomial = QC.forAll genNewtonCase $ \(n, points, xTest, coeffs) ->
    let poly x = sum [c * x ^ i | (c, i) <- zip coeffs [0 ..]]
        expected = poly xTest
     in case newtonValue n points xTest of
            Nothing -> QC.counterexample "newtonValue вернула Nothing" False
            Just actual ->
                QC.counterexample ("ожидалось " ++ show expected ++ ", получили " ++ show actual) $
                    approxEqual expected actual

-- | Тест одновременной работы двух алгоритмов (из модуля App)
simultaneousTest :: Test
simultaneousTest = TestLabel "simultaneous algorithms" $ TestCase $ do
    let points = [Point 0 0, Point 1 1, Point 2 4, Point 3 9]
        outputs = simulateSequential 0.5 [AlgLinear, AlgNewton 4] points
        linearActual = [(x, y) | (AlgLinear, x, y) <- outputs]
        newtonActual = [(x, y) | (AlgNewton _, x, y) <- outputs]
        linearExpected =
            [ (0.0, 0.0)
            , (0.5, 0.5)
            , (1.0, 1.0)
            , (1.5, 2.5)
            , (2.0, 4.0)
            , (2.5, 6.5)
            , (3.0, 9.0)
            ]
        newtonExpected =
            [ (0.0, 0.0)
            , (0.5, 0.25)
            , (1.0, 1.0)
            , (1.5, 2.25)
            , (2.0, 4.0)
            , (2.5, 6.25)
            , (3.0, 9.0)
            ]
    assertEqual
        "линейный алгоритм выдал нужное число значений"
        (length linearExpected)
        (length linearActual)
    assertEqual
        "алгоритм Ньютона выдал нужное число значений"
        (length newtonExpected)
        (length newtonActual)
    zipWithM_ (assertApproxPair "линейный алгоритм") linearExpected linearActual
    zipWithM_ (assertApproxPair "алгоритм Ньютона") newtonExpected newtonActual

-- | Список всех тестов
tests :: Test
tests =
    TestList
        [ linearUnitTests
        , newtonUnitTests
        , qcPropertyTest "linear interpolates affine line" prop_linearInterpolatesLine
        , qcPropertyTest "linear outside interval" prop_linearOutside
        , qcPropertyTest "newton matches polynomial" prop_newtonMatchesPolynomial
        , simultaneousTest
        ]
