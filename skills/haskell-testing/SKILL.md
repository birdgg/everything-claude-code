---
name: haskell-testing
description: Haskell testing patterns including HSpec, QuickCheck, property-based testing, and coverage. Follows TDD methodology with idiomatic Haskell practices.
---

# Haskell Testing Patterns

Comprehensive Haskell testing patterns for writing reliable, maintainable tests following TDD methodology.

## When to Activate

- Writing new Haskell functions or modules
- Adding test coverage to existing code
- Creating property-based tests with QuickCheck
- Implementing fuzz tests for parsing/validation
- Following TDD workflow in Haskell projects

## TDD Workflow for Haskell

### The RED-GREEN-REFACTOR Cycle

```
RED     → Write a failing test first
GREEN   → Write minimal code to pass the test
REFACTOR → Improve code while keeping tests green
REPEAT  → Continue with next requirement
```

### Step-by-Step TDD in Haskell

```haskell
-- Step 1: Define the type signature
-- src/Calculator.hs
module Calculator (add) where

add :: Int -> Int -> Int
add = error "not implemented"

-- Step 2: Write failing test (RED)
-- test/CalculatorSpec.hs
module CalculatorSpec (spec) where

import Test.Hspec
import Calculator

spec :: Spec
spec = describe "add" $ do
  it "adds two positive numbers" $
    add 2 3 `shouldBe` 5

-- Step 3: Run test - verify FAIL
-- $ stack test
-- CalculatorSpec
--   add
--     adds two positive numbers FAILED [1]
-- Failures:
--   1) add adds two positive numbers
--      error: not implemented

-- Step 4: Implement minimal code (GREEN)
add :: Int -> Int -> Int
add a b = a + b

-- Step 5: Run test - verify PASS
-- $ stack test
-- CalculatorSpec
--   add
--     adds two positive numbers
-- Finished in 0.0001 seconds
-- 1 example, 0 failures

-- Step 6: Refactor if needed, verify tests still pass
```

## HSpec - BDD Style Testing

### Basic Structure

```haskell
module MyModuleSpec (spec) where

import Test.Hspec
import MyModule

spec :: Spec
spec = do
  describe "functionName" $ do
    context "when given valid input" $ do
      it "returns expected result" $
        functionName validInput `shouldBe` expectedOutput

      it "handles edge case" $
        functionName edgeCase `shouldSatisfy` isValid

    context "when given invalid input" $ do
      it "returns error" $
        functionName invalidInput `shouldBe` Left SomeError
```

### Matchers

```haskell
spec :: Spec
spec = describe "matchers" $ do
  -- Equality
  it "shouldBe for equality" $
    1 + 1 `shouldBe` 2

  -- Predicate
  it "shouldSatisfy for predicates" $
    [1,2,3] `shouldSatisfy` (not . null)

  -- Comparison
  it "shouldReturn for IO actions" $
    readFile "test.txt" `shouldReturn` "content"

  -- Exception testing
  it "shouldThrow for exceptions" $
    evaluate (head []) `shouldThrow` anyException

  it "shouldThrow specific exception" $
    throwIO DivideByZero `shouldThrow` (== DivideByZero)

  -- Approximate equality
  it "shouldSatisfy for approximate" $
    3.14159 `shouldSatisfy` (\x -> abs (x - pi) < 0.001)

  -- List membership
  it "shouldContain for lists" $
    [1,2,3] `shouldContain` [2]

  -- Not equal
  it "shouldNotBe for inequality" $
    1 `shouldNotBe` 2

  -- Match patterns
  it "shouldMatchList ignores order" $
    [1,2,3] `shouldMatchList` [3,1,2]
```

### Hooks: Setup and Teardown

```haskell
spec :: Spec
spec = do
  -- Run before each test
  beforeAll (connectDatabase) $ do
    describe "database tests" $ do
      it "queries work" $ \conn ->
        runQuery conn "SELECT 1" `shouldReturn` [1]

  -- Run before each test in group
  before (createTempFile) $ do
    describe "file tests" $ do
      it "reads file" $ \path ->
        readFile path `shouldReturn` ""

  -- Cleanup after each test
  after cleanup $ do
    describe "resource tests" $ do
      it "uses resource" $
        useResource `shouldReturn` ()

  -- Around for bracket-style
  around (withDatabase) $ do
    describe "bracketed tests" $ do
      it "runs with resource" $ \db ->
        queryDb db `shouldReturn` []
```

### Pending and Focused Tests

```haskell
spec :: Spec
spec = describe "work in progress" $ do
  it "completed test" $
    True `shouldBe` True

  -- Mark as pending
  pending "not yet implemented"

  -- Pending with reason
  pendingWith "waiting for API fix"

  -- Focus during development (run only this)
  fit "focused test" $
    True `shouldBe` True

  -- Focus a describe block
  fdescribe "focused group" $ do
    it "runs this" $ True `shouldBe` True

  -- Skip tests
  xit "skipped test" $
    True `shouldBe` False
```

## QuickCheck - Property-Based Testing

### Basic Properties

```haskell
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "reverse" $ do
  -- Property: reverse is involutive
  prop "reverse . reverse = id" $
    \xs -> reverse (reverse xs) == (xs :: [Int])

  -- Property: reverse preserves length
  prop "preserves length" $
    \xs -> length (reverse xs) == length (xs :: [Int])

  -- Property with precondition
  prop "head of reversed is last" $
    \xs -> not (null xs) ==>
      head (reverse xs) == last (xs :: [Int])
```

### Custom Generators

```haskell
import Test.QuickCheck

-- Newtype with custom Arbitrary instance
newtype PositiveInt = PositiveInt Int
  deriving (Show, Eq)

instance Arbitrary PositiveInt where
  arbitrary = PositiveInt . abs <$> arbitrary `suchThat` (> 0)
  shrink (PositiveInt n) = PositiveInt <$> filter (> 0) (shrink n)

-- Generator combinators
genEmail :: Gen Text
genEmail = do
  local  <- listOf1 (elements ['a'..'z'])
  domain <- listOf1 (elements ['a'..'z'])
  tld    <- elements ["com", "org", "net"]
  pure $ T.pack $ local ++ "@" ++ domain ++ "." ++ tld

genUser :: Gen User
genUser = User
  <$> (NonEmptyText <$> listOf1 arbitrary)
  <*> genEmail
  <*> chooseInt (18, 100)

-- Use in properties
prop "user validation" $
  forAll genUser $ \user ->
    isRight (validateUser user)
```

### Shrinking

```haskell
-- Custom shrinking for better error messages
instance Arbitrary MyType where
  arbitrary = -- ...
  shrink (MyType a b) =
    [ MyType a' b | a' <- shrink a ] ++
    [ MyType a b' | b' <- shrink b ]

-- Shrinking finds minimal counterexample
-- Instead of: [1,2,3,4,5,6,7,8,9,10]
-- Reports:    [1] (minimal failing case)
```

### QuickCheck Modifiers

```haskell
spec :: Spec
spec = describe "with modifiers" $ do
  -- Non-empty lists
  prop "non-empty head" $
    \(NonEmpty xs) -> head xs == head (xs :: [Int])

  -- Positive numbers
  prop "positive sqrt" $
    \(Positive n) -> sqrt n >= (0 :: Double)

  -- ASCII strings
  prop "ASCII processing" $
    \(ASCIIString s) -> all isAscii s

  -- Small values (faster tests)
  prop "small lists" $
    \(Small n) -> length (replicate n ()) == n

  -- Ordered lists
  prop "sorted lists" $
    \(Sorted xs) -> xs == sort (xs :: [Int])

  -- Fixed size
  prop "fixed size list" $
    forAll (vectorOf 10 arbitrary) $ \xs ->
      length (xs :: [Int]) == 10
```

### Combining HSpec and QuickCheck

```haskell
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "MyModule" $ do
  -- Unit tests
  context "unit tests" $ do
    it "handles specific case" $
      myFunction 42 `shouldBe` expected

  -- Property tests
  context "properties" $ do
    prop "identity property" $
      \x -> myFunction (myInverse x) == (x :: Int)

    -- Adjust test count
    modifyMaxSuccess (* 10) $
      prop "thorough property" $
        \x -> expensiveProperty x

    -- Adjust size
    modifyMaxSize (const 50) $
      prop "bounded size property" $
        \xs -> boundedProperty (xs :: [Int])
```

## Tasty - Test Framework Integration

### Basic Tasty Setup

```haskell
-- test/Main.hs
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified MyModuleTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
  [ unitTests
  , propertyTests
  , MyModuleTest.tests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "addition" $
      1 + 1 @?= 2

  , testCase "comparison" $
      assertBool "should be true" (1 < 2)

  , testCase "with message" $
      assertEqual "values should match" expected actual
  ]

propertyTests :: TestTree
propertyTests = testGroup "Properties"
  [ testProperty "reverse reverse" $
      \xs -> reverse (reverse xs) == (xs :: [Int])

  , testProperty "sort idempotent" $
      \xs -> sort (sort xs) == sort (xs :: [Int])
  ]
```

### Tasty Options and Patterns

```haskell
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Runners

main :: IO ()
main = defaultMainWithIngredients ingredients tests
  where
    ingredients =
      [ listingTests
      , consoleTestReporter
      ]

-- Run specific tests
-- stack test --test-arguments="-p reverse"

-- List tests
-- stack test --test-arguments="--list-tests"

-- Set QuickCheck iterations
-- stack test --test-arguments="--quickcheck-tests=1000"
```

## Testing Monadic Code

### Testing IO Actions

```haskell
spec :: Spec
spec = describe "IO actions" $ do
  it "reads file" $ do
    content <- readFile "testdata/input.txt"
    content `shouldBe` "expected content"

  it "shouldReturn for IO" $
    getEnv "HOME" `shouldReturn` "/home/user"

  it "creates and cleans temp file" $ do
    result <- bracket
      (writeFile "temp.txt" "data")
      (\_ -> removeFile "temp.txt")
      (\_ -> readFile "temp.txt")
    result `shouldBe` "data"
```

### Testing with Mocks

```haskell
-- Define interface as type class
class Monad m => MonadDatabase m where
  queryUser :: UserId -> m (Maybe User)
  saveUser  :: User -> m ()

-- Production implementation
instance MonadDatabase IO where
  queryUser = realQueryUser
  saveUser  = realSaveUser

-- Test implementation
newtype TestM a = TestM { runTestM :: State TestState a }
  deriving (Functor, Applicative, Monad, MonadState TestState)

data TestState = TestState
  { users :: Map UserId User
  }

instance MonadDatabase TestM where
  queryUser uid = gets (Map.lookup uid . users)
  saveUser user = modify (\s -> s { users = Map.insert (userId user) user (users s) })

-- Test using mock
spec :: Spec
spec = describe "user service" $ do
  it "finds existing user" $ do
    let initialState = TestState (Map.singleton "123" testUser)
        result = evalState (runTestM $ queryUser "123") initialState
    result `shouldBe` Just testUser
```

### Testing StateT/ReaderT

```haskell
spec :: Spec
spec = describe "stateful computation" $ do
  it "updates state correctly" $ do
    let initial = AppState 0
        final = execState myComputation initial
    stateValue final `shouldBe` 42

  it "reads config correctly" $ do
    let config = AppConfig "test"
        result = runReader myReader config
    result `shouldBe` expectedResult
```

## Test Organization

### Directory Structure

```text
my-project/
├── src/
│   └── MyProject/
│       ├── Types.hs
│       └── Core.hs
├── test/
│   ├── Main.hs              # Test runner
│   ├── Spec.hs              # HSpec discovery
│   └── MyProject/
│       ├── TypesSpec.hs     # Tests for Types
│       └── CoreSpec.hs      # Tests for Core
├── my-project.cabal
└── stack.yaml
```

### HSpec Auto-Discovery

```haskell
-- test/Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

-- This automatically finds all *Spec.hs files
-- Each spec module must export: spec :: Spec
```

### Cabal Test Configuration

```cabal
test-suite my-project-test
  type:                exitcode-stdio-1.0
  main-is:             Spec.hs
  hs-source-dirs:      test
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
  build-depends:
    , base
    , my-project
    , hspec
    , hspec-discover
    , QuickCheck
  default-language:    Haskell2010
```

## Coverage

### Running with Coverage

```bash
# Stack
stack test --coverage

# View HTML report
stack hpc report --all --destdir=coverage

# Cabal
cabal test --enable-coverage
cabal hpc report

# Show coverage summary
stack hpc report
```

### Coverage Targets

| Code Type | Target |
|-----------|--------|
| Pure functions | 100% |
| Public API | 90%+ |
| General code | 80%+ |
| Template Haskell | Exclude |

### Excluding from Coverage

```haskell
-- Use HPC pragmas to exclude
{-# ANN module "HLint: ignore" #-}

-- Or exclude specific files in cabal
test-suite my-test
  ghc-options: -fhpc-coverage-exclude=Generated
```

## Testing Best Practices

**DO:**
- Write tests FIRST (TDD)
- Use property-based tests for pure functions
- Test behavior, not implementation
- Use meaningful test descriptions
- Test edge cases (empty, Nothing, Left)
- Keep tests independent and deterministic
- Use type-driven development alongside TDD

**DON'T:**
- Test internal/unexported functions directly
- Use `unsafePerformIO` in tests
- Write tests that depend on execution order
- Ignore shrunk counterexamples (they're minimal!)
- Mock everything (prefer pure functions)
- Skip testing error cases

## Haskell Testing Tips

```haskell
-- Use specific assertions
it "returns Right" $
  parseConfig input `shouldSatisfy` isRight

-- Test Left/Right specifically
it "returns specific error" $
  parseConfig bad `shouldBe` Left (ParseError "invalid")

-- Use shouldContain for partial matches
it "error message contains info" $
  show err `shouldContain` "line 42"

-- QuickCheck: always specify types
prop "works" $
  \xs -> myFunc xs == expected (xs :: [Int])

-- Use counterexample for debugging
prop "complex property" $
  \x -> counterexample (show (debug x)) $
    property $ complexPredicate x
```

## Testing Commands

```bash
# Run all tests
stack test

# Run specific test file
stack test --test-arguments="-m TypesSpec"

# Run matching tests
stack test --test-arguments="-m 'validates email'"

# Run with verbose output
stack test --test-arguments="--verbose"

# Run with coverage
stack test --coverage

# Run QuickCheck with more tests
stack test --test-arguments="--quickcheck-tests=1000"

# List all tests
stack test --test-arguments="--list-tests"

# Re-run only failed tests
stack test --test-arguments="--rerun"

# Run tests on file change
stack test --file-watch
```

## Integration with CI/CD

```yaml
# GitHub Actions example
test:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
    - uses: haskell-actions/setup@v2
      with:
        ghc-version: '9.6'
        enable-stack: true

    - name: Run tests
      run: stack test --coverage

    - name: Check coverage
      run: |
        stack hpc report | grep -E "^\s+\d+%" | \
        awk '{gsub(/%/,""); if ($1 < 80) exit 1}'
```

**Remember**: In Haskell, types are your first line of defense. Tests verify behavior that types cannot encode. Use QuickCheck to discover edge cases you didn't think of.
