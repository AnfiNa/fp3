## Лабораторная работа 3: Интерполяция


**Дисциплина:** Функциональное программирование  
**Студент:** Анфилатова Надежда Геннадьевна  
**Группа:** P3321

---
## Требования к разработанному ПО, включая описание алгоритма

### Назначение программы
Программа предназначена для интерполяции функций, заданных дискретным набором точек, с использованием двух методов: кусочно-линейной интерполяции и интерполяции многочленом Ньютона с заданной степенью. Пользователь может выбрать один или несколько алгоритмов, указать шаг дискретизации, а программа построит значения интерполянтов для всех точек от минимальной до максимальной с этим шагом.

### Функциональные требования
- Поддержка двух алгоритмов интерполяции: линейного (`--linear`) и Ньютона степени n (`--newton n`).
- Возможность задания шага дискретизации (`--step s`).
- Обработка аргументов командной строки, включая проверку корректности ввода (целое число ≥2 для Ньютона, положительный шаг).
- При указании нескольких алгоритмов они выполняются параллельно (каждый независимо) над одним и тем же набором точек.
- Программа читает входные точки из стандартного ввода и выводит результаты интерполяции в формате: `алгоритм: x y` для каждого сгенерированного значения.
- Интерполяция выполняется последовательно по мере поступления новых точек: при добавлении каждой новой точки пересчитываются значения для всех ещё не обработанных узлов дискретизации, начиная с последнего вычисленного.

### Описание алгоритмов

#### Линейная интерполяция (linearValue)
Реализует кусочно-линейную интерполяцию. Для заданного значения `x` ищется отрезок `[x_i, x_{i+1}]`, содержащий `x`. Если `x` лежит вне диапазона известных точек, возвращается `Nothing`. Если отрезок найден, значение вычисляется по формуле линейной интерполяции:
\[
y = y_i + \frac{y_{i+1} - y_i}{x_{i+1} - x_i} (x - x_i).
\]

#### Интерполяция Ньютона (newtonValue)
Реализует интерполяционный многочлен Ньютона с разделенными разностями степени `n-1` (используется `n` точек). Для заданного `x` выбирается окно из `n` ближайших точек (с учётом положения `x` относительно известных абсцисс). Затем вычисляются разделенные разности по формулам:
\[
f[x_i] = y_i,\quad f[x_i, x_{i+1}] = \frac{y_{i+1} - y_i}{x_{i+1} - x_i},\quad \ldots
\]
После получения коэффициентов `c_k` многочлен вычисляется по схеме Горнера (в коде — сумма произведений):
\[
P(x) = c_0 + c_1(x - x_0) + c_2(x - x_0)(x - x_1) + \ldots + c_{n-1}(x - x_0)\ldots(x - x_{n-2}).
\]

Если доступно меньше `n` точек, интерполяция не производится (`Nothing`).

## Ключевые элементы реализации

### Типы данных
```haskell
-- Алгоритмы интерполяции
data Algorithm = AlgLinear | AlgNewton Int

-- Состояние алгоритма: текущий алгоритм и следующая точка для вычисления
data AlgoState = AlgoState Algorithm (Maybe Double)

-- Результат интерполяции: алгоритм, x, y
type Output = (Algorithm, Double, Double)
```

### Парсинг аргументов
```haskell
parseArgs :: [String] -> Either String Options
parseArgs = foldl walk defaultOptions >>= checkResult
  where
    walk opts ("--linear":rest) = addAlgorithm AlgLinear opts
    walk opts ("--newton":v:rest) = case readMaybe v of Just n | n>=2 -> addAlgorithm (AlgNewton n) opts
    walk opts ("--step":v:rest) = case readMaybe v of Just s | s>0 -> opts {optStep = s}
    walk _ ("--help":_) = Left ""
    walk _ (unknown:_) = Left ("Неизвестный аргумент: " ++ unknown)
    addAlgorithm alg opts = if alg `elem` optAlgorithms opts then opts else opts {optAlgorithms = optAlgorithms opts ++ [alg]}
    checkResult opts | null (optAlgorithms opts) = Left "Нужно указать алгоритм" | otherwise = Right opts
```

### Интерполяция

#### Линейная
```haskell
linearValue :: [Point] -> Double -> Maybe Double
linearValue (p1:p2:rest) x
  | x < x1 = Nothing
  | x <= x2 && x2 > x1 = Just (y1 + (y2-y1)*(x-x1)/(x2-x1))
  | otherwise = linearValue (p2:rest) x
```

#### Ньютон
```haskell
newtonValue :: Int -> [Point] -> Double -> Maybe Double
newtonValue n points x = do
  window <- pickWindow n x points
  return (evaluateNewton window x)

pickWindow n x pts = Just (take n (drop start pts)) where
  xs = map px pts; len = length pts; pos = length (takeWhile (<= x) xs)
  half = n `div` 2; rawStart = pos - half; maxStart = len - n
  start = max 0 (min rawStart maxStart)

evaluateNewton pts x = sum (zipWith term coeffs [0..]) where
  xs = map px pts; ys = map py pts
  coeffs = dividedDifferences xs ys
  term c idx = c * product [x - xs!!j | j <- [0..idx-1]]

dividedDifferences xs ys = go ys 0 where
  n = length xs
  go [] _ = []
  go (c:current) level | level>=n = []
                        | otherwise = c : go next (level+1)
    where next = [ (current!!(i+1) - current!!i) / (xs!!(i+level+1) - xs!!i) | i <- [0..n-level-2] ]
```

### Управление состояниями и генерация вывода
```haskell
-- Начальное состояние: ни один алгоритм ещё не выдал значений
initialAlgoStates algorithms = [AlgoState alg Nothing | alg <- algorithms]

-- Продвинуть все алгоритмы с учётом нового набора точек
advanceAlgorithms step pts states =
  let results = map (advanceOne step pts) states
  in (map fst results, concatMap snd results)

-- Для одного алгоритма: сгенерировать значения от nextX до lastX с шагом step
advanceOne step pts (AlgoState kind nextX) =
  let startX = fromMaybe (px $ head pts) nextX
      lastX = px (last pts)
      generateNext current
        | current > lastX = Nothing
        | otherwise = do val <- calculate kind pts current
                         return ((kind, current, val), current + step)
      outputs = unfoldr generateNext startX
      finalX = if null outputs then startX else (\(_,x,_) -> x) (last outputs) + step
  in (AlgoState kind (Just finalX), outputs)

-- Последовательная обработка точек: накопление состояний и результатов
simulateSequential step algorithms points =
  let initial = initialAlgoStates algorithms
      stepFunc (accPoints, states, out) point =
        let newAcc = accPoints ++ [point]
            (newStates, newOut) = advanceAlgorithms step newAcc states
        in (newAcc, newStates, out ++ newOut)
  in (\(_,_,final) -> final) $ foldl stepFunc ([], initial, []) points
```

## Ввод/вывод программы

### Входные данные
Программа получает настройки через аргументы командной строки:
- `--linear` — включить линейную интерполяцию.
- `--newton n` — включить интерполяцию Ньютона степени n (n ≥ 2).
- `--step s` — задать шаг дискретизации (s > 0, по умолчанию 1.0).

Точки для интерполяции, предположительно, подаются в стандартный ввод в виде пар чисел (x y), по одной паре на строку. В представленном коде чтение точек не показано, но подразумевается, что они будут прочитаны и преобразованы в список `[Point]` перед вызовом `simulateSequential`.

### Выходные данные
Программа выводит строки вида:
```
linear: 1.5 2.7
newton: 1.5 2.8
```
где первое слово — имя алгоритма, затем координата x и интерполированное значение y. Вывод происходит для каждого алгоритма и каждой точки дискретизации, попадающей в интервал известных абсцисс.

## Выводы

### Достоинства подхода
1. **Функциональный стиль** обеспечивает отсутствие побочных эффектов, упрощает тестирование и отладку. Все функции чисты, состояние явно передаётся через аргументы и возвращаемые значения.
2. **Использование алгебраических типов** (`Algorithm`, `AlgoState`) позволяет компактно моделировать предметную область и гарантировать, что обрабатываются только допустимые варианты.
3. **Модульность** (разделение на `App` и `Interpolation`) чётко отделяет логику управления от математических алгоритмов, что облегчает добавление новых методов интерполяции.
4. **Ленивые списки и `unfoldr`** эффективно генерируют последовательности значений без необходимости явного управления циклами и накопления результатов.
5. **Безопасная обработка ошибок** через `Either` и `Maybe` предотвращает необработанные исключения и явно указывает на возможные проблемы (недостаточно точек, выход за границы).

Запуск: cabal run fp-lab3 -- --step 0.5 --linear