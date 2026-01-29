---
description: Enforce TDD workflow for Haskell. Define types first, write HSpec/QuickCheck tests, then implement. Verify coverage with hpc.
---

# Haskell TDD Command

This command enforces test-driven development methodology for Haskell code using idiomatic testing patterns with HSpec, QuickCheck, and Tasty.

## What This Command Does

1. **Define Types**: Scaffold type signatures and data types first
2. **Write Tests**: Create HSpec specs or QuickCheck properties (RED)
3. **Run Tests**: Verify tests fail for the right reason
4. **Implement Code**: Write minimal code to pass (GREEN)
5. **Refactor**: Improve while keeping tests green
6. **Check Coverage**: Ensure adequate coverage with hpc

## When to Use

Use `/haskell-test` when:
- Implementing new Haskell functions
- Adding test coverage to existing code
- Fixing bugs (write failing test first)
- Building critical business logic
- Writing pure functions with QuickCheck properties
- Learning TDD workflow in Haskell

## TDD Cycle

```
RED     → Write failing HSpec spec or QuickCheck property
GREEN   → Implement minimal code to pass
REFACTOR → Improve code, tests stay green
REPEAT  → Next test case
```

## Example Session

```text
User: /haskell-test I need a function to validate email addresses

Agent:
# TDD Session: Email Validator

## Step 1: Define Types

```haskell
-- src/Validator/Email.hs
module Validator.Email
  ( validateEmail
  , EmailError(..)
  ) where

data EmailError
  = EmailEmpty
  | EmailInvalid String
  deriving (Show, Eq)

-- | Validate an email address.
-- Returns Right () if valid, Left EmailError otherwise.
validateEmail :: String -> Either EmailError ()
validateEmail = error "not implemented"
```

## Step 2: Write HSpec Tests (RED)

```haskell
-- test/Validator/EmailSpec.hs
module Validator.EmailSpec (spec) where

import Test.Hspec
import Validator.Email

spec :: Spec
spec = describe "validateEmail" $ do
  -- Valid emails
  context "with valid emails" $ do
    it "accepts simple email" $
      validateEmail "user@example.com" `shouldBe` Right ()

    it "accepts email with subdomain" $
      validateEmail "user@mail.example.com" `shouldBe` Right ()

    it "accepts email with plus" $
      validateEmail "user+tag@example.com" `shouldBe` Right ()

    it "accepts email with dots" $
      validateEmail "first.last@example.com" `shouldBe` Right ()

  -- Invalid emails
  context "with invalid emails" $ do
    it "rejects empty string" $
      validateEmail "" `shouldBe` Left EmailEmpty

    it "rejects missing at sign" $
      validateEmail "userexample.com" `shouldSatisfy` isLeft

    it "rejects missing domain" $
      validateEmail "user@" `shouldSatisfy` isLeft

    it "rejects missing local part" $
      validateEmail "@example.com" `shouldSatisfy` isLeft

    it "rejects double at" $
      validateEmail "user@@example.com" `shouldSatisfy` isLeft

    it "rejects spaces" $
      validateEmail "user @example.com" `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
```

## Step 3: Run Tests - Verify FAIL

```bash
$ stack test

Validator.Email
  validateEmail
    with valid emails
      accepts simple email FAILED [1]
    ...

Failures:

  1) Validator.EmailSpec.validateEmail.with valid emails accepts simple email
       uncaught exception: ErrorCall
       not implemented

FAIL
```

✓ Tests fail as expected (not implemented error).

## Step 4: Implement Minimal Code (GREEN)

```haskell
-- src/Validator/Email.hs
module Validator.Email
  ( validateEmail
  , EmailError(..)
  ) where

import Data.Char (isAlphaNum)
import Data.List (elemIndex)

data EmailError
  = EmailEmpty
  | EmailInvalid String
  deriving (Show, Eq)

validateEmail :: String -> Either EmailError ()
validateEmail "" = Left EmailEmpty
validateEmail email =
  case break (== '@') email of
    ("", _) -> Left $ EmailInvalid "missing local part"
    (_, "") -> Left $ EmailInvalid "missing @ sign"
    (_, "@") -> Left $ EmailInvalid "missing domain"
    (local, '@':domain)
      | ' ' `elem` email -> Left $ EmailInvalid "contains spaces"
      | '@' `elem` domain -> Left $ EmailInvalid "multiple @ signs"
      | '.' `notElem` domain -> Left $ EmailInvalid "missing TLD"
      | otherwise -> Right ()
    _ -> Left $ EmailInvalid "invalid format"
```

## Step 5: Run Tests - Verify PASS

```bash
$ stack test

Validator.Email
  validateEmail
    with valid emails
      accepts simple email
      accepts email with subdomain
      accepts email with plus
      accepts email with dots
    with invalid emails
      rejects empty string
      rejects missing at sign
      rejects missing domain
      rejects missing local part
      rejects double at
      rejects spaces

Finished in 0.0012 seconds
10 examples, 0 failures

PASS
```

✓ All tests passing!

## Step 6: Check Coverage

```bash
$ stack test --coverage

Generating coverage report...
Writing: hpc_index.html

Package coverage: 95%
  Validator.Email: 100%
```

✓ Coverage: 100%

## TDD Complete!
```

## Test Patterns

### HSpec - BDD Style Tests

```haskell
spec :: Spec
spec = describe "ModuleName" $ do
  context "when condition" $ do
    it "should behave correctly" $ do
      functionUnderTest input `shouldBe` expectedOutput

    it "should handle edge case" $ do
      functionUnderTest edge `shouldSatisfy` predicate
```

### QuickCheck - Property-Based Tests

```haskell
spec :: Spec
spec = describe "reverse" $ do
  it "is involutive" $ property $
    \xs -> reverse (reverse xs) == (xs :: [Int])

  it "preserves length" $ property $
    \xs -> length (reverse xs) == length (xs :: [Int])

  it "is identity for palindromes" $ property $
    \xs -> let pal = xs ++ reverse xs
           in reverse pal == (pal :: [Int])
```

### QuickCheck with Custom Generators

```haskell
newtype PositiveInt = PositiveInt Int
  deriving (Show, Eq)

instance Arbitrary PositiveInt where
  arbitrary = PositiveInt . abs <$> arbitrary `suchThat` (> 0)

spec :: Spec
spec = describe "sqrt'" $ do
  it "returns non-negative for positive inputs" $ property $
    \(PositiveInt n) -> sqrt' n >= 0
```

### Tasty - Test Framework Integration

```haskell
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Unit Tests"
    [ testCase "simple case" $
        validateEmail "user@example.com" @?= Right ()
    ]
  , testGroup "Properties"
    [ testProperty "roundtrip" $
        \x -> decode (encode x) == Just (x :: MyType)
    ]
  ]
```

### Test Fixtures and Setup

```haskell
spec :: Spec
spec = beforeAll setupDB $ do
  describe "database operations" $ do
    it "inserts correctly" $ \conn -> do
      result <- insertUser conn testUser
      result `shouldBe` Right 1

setupDB :: IO Connection
setupDB = do
  conn <- connect defaultConnectInfo
  runMigrations conn
  return conn
```

## Coverage Commands

```bash
# Run tests with coverage (Stack)
stack test --coverage

# Run tests with coverage (Cabal)
cabal test --enable-coverage

# Generate HTML report
stack hpc report --all --destdir=coverage

# View coverage by module
stack hpc report

# Coverage for specific test suite
stack test :my-test-suite --coverage
```

## Coverage Targets

| Code Type | Target |
|-----------|--------|
| Pure functions | 100% |
| Public API | 90%+ |
| General code | 80%+ |
| Generated/TH code | Exclude |

## TDD Best Practices

**DO:**
- Write test FIRST, before any implementation
- Define types and signatures before writing tests
- Use QuickCheck for pure functions with invariants
- Use HSpec for behavior-driven specifications
- Test edge cases (empty, Nothing, Left, bottom)
- Keep tests independent and isolated

**DON'T:**
- Write implementation before tests
- Skip the RED phase
- Test internal/unexported functions directly
- Use `unsafePerformIO` in tests
- Ignore property test failures (shrink to minimal case)
- Write tests that depend on execution order

## Haskell Testing Tips

- **Use `shouldBe` for exact equality**: Tests are clearer
- **Use `shouldSatisfy` for predicates**: When exact value doesn't matter
- **Use `shouldThrow` for exceptions**: Test error conditions
- **Use `property` for QuickCheck in HSpec**: Integrates smoothly
- **Use `pending` for unfinished tests**: Mark work in progress
- **Use `focus` sparingly**: Run specific tests during development

## Related Commands

- `/haskell-build` - Fix build errors
- `/haskell-review` - Review code after implementation
- `/verify` - Run full verification loop

## Related

- Agent: `agents/haskell-reviewer.md`
- Agent: `agents/haskell-build-resolver.md`
