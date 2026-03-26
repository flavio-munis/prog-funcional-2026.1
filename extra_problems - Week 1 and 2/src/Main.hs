import ExtraProblems
import System.Random
import Data.Numbers.Primes (primes)

-- Define types alias for tests
type Test = (String, Bool)
type TestSuite = (String, [Test])

-- Rum a Test Suite and Return it's results in a string and total test passed as a int.
runTest :: Test -> (String, Int)
runTest (name, True) = ((name ++ " - Passed ✅"), 1)
runTest (name, _)    = ((name ++ " - Failed ❌"), 0)

-- Run a test suite and return a string with the results and the numbers of tests passed
runTestSuite :: TestSuite -> (String, Int)
runTestSuite ("", _)       = ("", 0)
runTestSuite (name, tests) = aux tests ("", 0)
  where
    total_passed passed = name ++
      " - Total Test Passed " ++ show passed ++ "/" ++ show (length tests) ++ "\n"
    aux [] (acc_str, acc_passed)     =
      (acc_str ++ (total_passed acc_passed) ++ "\n", acc_passed)
    aux (t:ts) (acc_str, acc_passed) =
      aux ts (acc_str ++ curr_str ++ "\n", acc_passed + passed)
      where
        (curr_str, passed) = runTest t

-- User defined Tests
testsAlternate :: [Test]
testsAlternate = 
    [ ("Alternate empty list",            alternate [] == 0)
    , ("Alternate single item (Int)",     alternate [1] == 1)
    , ("Alternate single item (Double)",  alternate [1.0] == 1.0)
    , ("Alternate two items",             alternate [1,1] == 0)
    , ("Alternate +2 items",              alternate [1.2,-1.5,36.0] == 38.7)
    ]

testsMinMax :: [Test]
testsMinMax =
  [ ("Min Max Single Item", min_max [1] == (1,1))
  , ("Min Max Two Items",   min_max ['a', 'b'] == ('a', 'b'))
  , ("Min Max +2 Items",    min_max ["a", "aaa", "ab", "aab"] == ("a", "ab"))]

testsCumsum :: [Test]
testsCumsum =
  [ ("Empty List",        cumsum [] == [])
  , ("One Element List",  cumsum [1] == [1])
  , ("Multiple Elements", cumsum [1,4,20] == [1,5,25])
  , ("Double",            cumsum [10.5,20.5,100.2] == [10.5,31.0,131.2])]

testsRepeat :: [Test]
testsRepeat =
  [ ("Empty Lists",      null (repeat' [] []))
  , ("Empty List 1",     null (repeat' [] [1]))
  , ("Empty List 2",     repeat' [1] [] == [1])
  , ("One Element List", repeat' [True] [2] == [True, True])
  , ("+2 Elements List", repeat' "abc" [3,2,1] == "aaabbc")]

testsHasTrue :: [Test]
testsHasTrue =
  [ ("Empty List",     not $ hasTrue [])
  , ("1 Element",      not $ hasTrue [False])
  , ("2 Element (1)",  hasTrue [False, True])
  , ("2 ELement (2)",  hasTrue [True, False])
  , ("2 ELement (3)",  not $ hasTrue [False, False])
  , ("+2 ELement (1)", hasTrue [False, True, False])
  , ("+2 ELement (2)", not $ hasTrue [False, False, False])]

testsAllTrue :: [Test]
testsAllTrue =
  [ ("Empty List",     allTrue [])
  , ("1 Element (1)",  not $ allTrue [False])
  , ("1 Element (1)",  allTrue [True])
  , ("2 Element (1)",  not $ allTrue [False, True])
  , ("2 ELement (2)",  not $ allTrue [True, False])
  , ("2 ELement (3)",  allTrue [True, True])
  , ("2 ELement (4)",  not $ allTrue [False, False])
  , ("+2 ELement (1)", not $ allTrue [False, True, False])
  , ("+2 ELement (2)", not $ allTrue [False, False, False])
  , ("+2 ELement (3)", allTrue [True, True, True])]

testsZip :: [Test]
testsZip =
  [ ("Empty Lists",           null $ zip' [] [])
  , ("One Empty List (1)",    null $ zip' [] [1])
  , ("One Empty List (2)",    null $ zip' "" [])
  , ("Same Length Lists (1)", zip' [Just 0] [1] == [(Just 0, 1)])
  , ("Same Length Lists (2)", zip' [1,2,3] "abc" == [(1,'a'), (2,'b'), (3,'c')])
  , ("Different Length (1)",  zip' [1,2,3] "a" == [(1,'a')])
  , ("Different Length (2)",  zip' [1] "abc" == [(1,'a')])]

testsZipRecycle :: [Test]
testsZipRecycle =
  [ ("Empty Lists",           null $ zipRecycle [] [])
  , ("One Empty List (1)",    null $ zipRecycle [] [1])
  , ("One Empty List (2)",    null $ zipRecycle "" [])
  , ("Same Length Lists (1)", zipRecycle [Just 0] [1] == [(Just 0, 1)])
  , ("Same Length Lists (2)", zipRecycle [1,2,3] "abc" == [(1,'a'), (2,'b'), (3,'c')])
  , ("Different Length (1)",  zipRecycle [1,2,3] "a" == [(1,'a'),(2,'a'),(3,'a')])
  , ("Different Length (2)",  zipRecycle [1] "abc" == [(1,'a'), (1,'b'),(1,'c')])
  , ("Different Lenght (3)", zipRecycle [1,2,3] [1,2,3,4,5,6,7] == [(1,1),(2,2),(3,3),(1,4),(2,5),(3,6),(1,7)])]

testsSplitUp :: [Test]
testsSplitUp =
  [ ("Empty List",        splitup [] == ([],[]))
  , ("One Element (1)",   splitup [1] == ([],[1]))
  , ("One Element (2)",   splitup [-1] == ([-1],[]))
  , ("One Element (3)",   splitup [0] == ([0],[]))
  , ("Two Elements (1)",  splitup [2,1] == ([],[2,1]))
  , ("Two Elements (2)",  splitup [-1,1] == ([-1],[1]))
  , ("Two Elements (3)",  splitup [-2,-1] == ([-2,-1],[]))
  , ("Two Elements (4)",  splitup [0,1] == ([0],[1]))
  , ("+Two Elements (1)", splitup [-2,1,0,2,-1,10] == ([-2,0,-1],[1,2,10]))]

testsSplitAt :: [Test]
testsSplitAt =
  [ ("Empty List",        splitat [] 0 == ([],[]))
  , ("One Element (1)",   splitat [1] 1 == ([1],[]))
  , ("One Element (2)",   splitat "a" 'b'  == ("a", ""))
  , ("One Element (3)",   splitat "b" 'a' == ("","b"))
  , ("Two Elements (1)",  splitat [2,1] 0 == ([],[2,1]))
  , ("Two Elements (2)",  splitat [-1,1] 0 == ([-1],[1]))
  , ("Two Elements (3)",  splitat [-2,-1] 0 == ([-2,-1],[]))
  , ("Two Elements (4)",  splitat [0,1] 0 == ([0],[1]))
  , ("+Two Elements (1)", splitat "zefgjkl" 'g' == ("efg","zjkl"))]

testsIsSorted :: [Test]
testsIsSorted =
  [ ("Empty List",            isSorted "")
  , ("One Element",           isSorted "a")
  , ("Two Elements (1)",      isSorted "ab")
  , ("Two Elements (2)",      not $ isSorted "ba")
  , ("Multiple Elements (1)", isSorted [-1,0,10])
  , ("Multiple Elements (2)", not $ isSorted [-1,0,10,2])]

testsIsAnySorted :: [Test]
testsIsAnySorted =
  [ ("Empty List",            isAnySorted "")
  , ("One Element",           isAnySorted "a")
  , ("Two Elements (1)",      isAnySorted "ab")
  , ("Two Elements (2)",      isAnySorted "ba")
  , ("Multiple Elements (1)", isAnySorted [-1,0,10])
  , ("Multiple Elements (2)", isAnySorted [2,1,-1])
  , ("Multiple Elements (3)", not $ isAnySorted [2,3,1,-1])]

-- Generate a list of 'n' integers between 'low' and 'high'
generateRandomList :: Int -> (Int, Int) -> Int -> [Int]
generateRandomList n range seed = take n (randomRs range (mkStdGen seed))

testsQSort :: [Test]
testsQSort =
  [ ("Empty List",  null $ qsort "")
  , ("Random List", isSorted $ qsort randomList)]
  where
    randomList = generateRandomList 1000 (1,100) seed
    seed = 42

testsDivide :: [Test]
testsDivide =
  [ ("Empty List",   divide "" == ([],[]))
  , ("One Element",  divide [1] == ([1],[]))
  , ("Two Element",  divide "ab" == ("a","b"))
  , ("+Two Element", divide [1..7] == ([1,3,5,7],[2,4,6]))]

testsNotSoQuickSort :: [Test]
testsNotSoQuickSort =
  [ ("Empty List",  null $ notSoQuickSort "")
  , ("Random List", isSorted $ notSoQuickSort randomList)]
  where
    randomList = generateRandomList 1000 (1,100) seed
    seed = 1337

testsFullDivide :: [Test]
testsFullDivide =
  [ ("Divisible",     fullDivide 2 40 == (3,5))
  , ("Not Divisible", fullDivide 3 10 == (0,10))]

testsFactorize :: [Test]
testsFactorize =
  [ ("Integer 1",             factorize 1 == [])
  , ("Integer 20",            factorize 20 == [(2,2),(5,1)])
  , ("Integer 36",            factorize 36 == [(2,2),(3,2)])
  , ("6-Digit Prime Number",  factorize 274177 == [(274177, 1)])
  , ("8-Digit Prime Number",  factorize 43349443 == [(43349443, 1)])
  , ("10-Digit Prime Number", factorize 1000000007 == [(1000000007,1)])
  , ("12-Digit Prime Number", factorize 100000000019 == [(100000000019,1)])
  , ("14-Digit Prime Number", factorize 100000000000031 == [(100000000000031,1)])
  , ("Lot of Factors Number", factorize manyFactorsNumber == factors)]
  where
    manyFactorsNumber = product $ take 10 primes
    factors = zip (take 10 primes) [1 | _ <- [1..10]]

testsMultiply :: [Test]
testsMultiply =
  [ ("Integer 1",             multiply (factorize 1) == 1)
  , ("Integer 20",            multiply (factorize 20) == 20)
  , ("Integer 36",            multiply (factorize 36) == 36)
  , ("6-Digit Prime Number",  multiply (factorize 274177) == 274177)
  , ("8-Digit Prime Number",  multiply (factorize 43349443) == 43349443)
  , ("10-Digit Prime Number", multiply (factorize 1000000007) == 1000000007)
  , ("Lot of Factors Number", multiply (factorize manyFactorsNumber) == manyFactorsNumber)]
  where
    manyFactorsNumber = product $ take 10 primes

testsAllDivisors :: [Test]
testsAllDivisors =
  [ ("Integer 1",      allDivisors (factorize 1) == [1])
  , ("Integer 20",     allDivisors (factorize 20) == [1,2,4,5,10,20])
  , ("Random Integer", allDivisors (factorize 543789) == [1,3,9,23,37,69,71,111,207,213,333,639,851,1633,2553,2627,4899,7659,7881,14697,23643,60421,181263,543789])]

suites :: [TestSuite]
suites = [ ("Alternate",      testsAlternate)
         , ("Min Max",        testsMinMax)
         , ("Cumsum",         testsCumsum)
         , ("Repeat",         testsRepeat)
         , ("hasTrue",        testsHasTrue)
         , ("allTrue",        testsAllTrue)
         , ("zip",            testsZip)
         , ("zipRecycle",     testsZipRecycle)
         , ("splitup",        testsSplitUp)
         , ("splitat",        testsSplitAt)
         , ("isSorted",       testsIsSorted)
         , ("isAnySorted",    testsIsAnySorted)
         , ("qsort",          testsQSort)
         , ("divide",         testsDivide)
         , ("notSoQuickSort", testsNotSoQuickSort)
         , ("fullDivide",     testsFullDivide)
         , ("factorize",      testsFactorize)
         , ("multiply",       testsMultiply)
         , ("allDivisors",    testsAllDivisors)]

-- Print all the suites results
main :: IO ()
main = mapM_ printTest testResults
  where
    testResults = map runTestSuite suites
    printTest :: (String, Int) -> IO ()
    printTest (str, _) = 
      putStr str
