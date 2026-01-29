---
name: haskell-relude-patterns
description: Relude library patterns for safer, more ergonomic Haskell development. Covers text handling, partial function alternatives, container types, and ecosystem integration.
---

# Haskell Relude Patterns

Relude is a modern alternative Haskell prelude that provides safe, ergonomic defaults. It removes partial functions, uses `Text` instead of `String`, and provides better container type ergonomics.

## When to Use This Skill

- Starting new Haskell projects
- Migrating from standard Prelude to Relude
- Working with projects that use Relude
- Reviewing Relude-based code

## Getting Started

### Package Configuration

```yaml
# package.yaml
dependencies:
  - base >= 4.14 && < 5
  - relude >= 1.0

default-extensions:
  - NoImplicitPrelude
  - OverloadedStrings

ghc-options:
  - -Wall
  - -Wcompat
```

```cabal
-- project.cabal
build-depends:
    base >= 4.14 && < 5
  , relude >= 1.0

default-extensions:
    NoImplicitPrelude
    OverloadedStrings
```

### Module Setup

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
module MyModule (someFunction) where

import Relude

someFunction :: Text -> IO ()
someFunction msg = putTextLn msg
```

### Recommended Language Extensions

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
```

## Text Handling

Relude uses `Text` as the default string type instead of `String` (`[Char]`).

### Why Text Over String

- `Text` is O(1) length, `String` is O(n)
- `Text` uses less memory (packed UTF-16)
- `Text` has better performance for most operations
- `Text` is the standard in modern Haskell libraries

### Basic Text Operations

```haskell
import Relude
import qualified Data.Text as T

-- Good: Using Text
processName :: Text -> Text
processName = T.toUpper . T.strip

-- Bad: Using String
processNameBad :: String -> String
processNameBad = map toUpper . dropWhile isSpace
```

### Text Conversions

```haskell
-- String <-> Text
toText   :: String -> Text
toString :: Text -> String

-- Text <-> Lazy Text
toLText   :: Text -> LText
toStrict  :: LText -> Text

-- Text <-> ByteString (UTF-8)
encodeUtf8 :: Text -> ByteString
decodeUtf8 :: ByteString -> Text

-- Safe decoding (handles invalid UTF-8)
decodeUtf8' :: ByteString -> Either UnicodeException Text
```

### Show Returns Text

```haskell
-- Relude's show returns Text, not String
show :: Show a => a -> Text

-- Example
displayNumber :: Int -> Text
displayNumber n = "The number is: " <> show n
```

## Eliminating Partial Functions

Relude removes dangerous partial functions and provides safe alternatives.

### Removed Partial Functions

These functions from Prelude are NOT available in Relude:

- `head`, `tail`, `init`, `last` (on lists)
- `fromJust`
- `read`
- `undefined` (use `error` or `bug` instead)
- `!!` (list indexing)

### Safe Alternatives

```haskell
-- head: Use NonEmpty or viaNonEmpty
head :: NonEmpty a -> a                    -- Total, requires NonEmpty
viaNonEmpty head :: [a] -> Maybe a         -- Safe, returns Maybe

-- Example
safeFirst :: [Int] -> Maybe Int
safeFirst = viaNonEmpty head

-- fromJust: Use fromMaybe or pattern matching
fromMaybe :: a -> Maybe a -> a
fromMaybe defaultVal Nothing  = defaultVal
fromMaybe _          (Just x) = x

-- Example
getUserName :: Maybe Text -> Text
getUserName = fromMaybe "Anonymous"

-- read: Use readMaybe
readMaybe :: Read a => String -> Maybe a

-- Example
parsePort :: Text -> Maybe Int
parsePort = readMaybe . toString

-- !!: Use indexing with Maybe
(!?) :: [a] -> Int -> Maybe a

-- Example
safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i = xs !? i
```

### Handling Maybe Values

```haskell
-- Convert Maybe to Either
maybeToRight :: e -> Maybe a -> Either e a
maybeToLeft  :: e -> Maybe a -> Either e a

-- Convert Either to Maybe
rightToMaybe :: Either e a -> Maybe a
leftToMaybe  :: Either e a -> Maybe e

-- Example: Validation
validateInput :: Text -> Either ValidationError User
validateInput input = do
  name <- maybeToRight EmptyName (parseNonEmpty input)
  pure (User name)
```

## NonEmpty Lists

Use `NonEmpty` to guarantee at least one element and enable safe head/tail.

### Creating NonEmpty

```haskell
import Data.List.NonEmpty (NonEmpty(..))

-- Constructor: x :| xs
example :: NonEmpty Int
example = 1 :| [2, 3, 4]

-- From list (may fail)
nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty []     = Nothing
nonEmpty (x:xs) = Just (x :| xs)

-- From list (total, with fallback)
fromMaybe (defaultVal :| []) (nonEmpty xs)
```

### Safe Operations on NonEmpty

```haskell
-- These are TOTAL functions on NonEmpty
head :: NonEmpty a -> a
tail :: NonEmpty a -> [a]
last :: NonEmpty a -> a
init :: NonEmpty a -> [a]

-- Example
processItems :: NonEmpty Item -> Result
processItems items = do
  let first = head items      -- Safe! No Maybe needed
  let rest  = tail items      -- Returns regular list
  process first rest
```

### Converting Between List and NonEmpty

```haskell
-- List to NonEmpty
nonEmpty :: [a] -> Maybe (NonEmpty a)

-- NonEmpty to List
toList :: NonEmpty a -> [a]

-- Pattern: Require NonEmpty in function signature
processNonEmpty :: NonEmpty Item -> Result

-- Call site must prove non-emptiness
main = case nonEmpty items of
  Nothing -> handleEmpty
  Just ne -> processNonEmpty ne
```

### viaNonEmpty Pattern

```haskell
-- Apply NonEmpty function to list, get Maybe result
viaNonEmpty :: (NonEmpty a -> b) -> [a] -> Maybe b

-- Examples
viaNonEmpty head :: [a] -> Maybe a
viaNonEmpty last :: [a] -> Maybe a
viaNonEmpty (foldl1 (+)) :: Num a => [a] -> Maybe a
```

## Container Types

Relude re-exports common container types with better defaults.

### HashMap and HashSet

```haskell
import Relude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

-- HashMap for fast O(1) lookup (unordered)
type UserCache = HashMap UserId User

lookupUser :: UserId -> UserCache -> Maybe User
lookupUser = HM.lookup

-- HashSet for membership testing
type ActiveUsers = HashSet UserId

isActive :: UserId -> ActiveUsers -> Bool
isActive = HS.member
```

### Map and Set (Ordered)

```haskell
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Map for ordered keys, range queries
type Timeline = Map Timestamp Event

getEventsInRange :: Timestamp -> Timestamp -> Timeline -> [Event]
getEventsInRange start end = Map.elems . Map.takeWhileAntitone (<= end) . Map.dropWhileAntitone (< start)

-- Set for ordered unique elements
type SortedTags = Set Tag
```

### IntMap and IntSet

```haskell
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

-- IntMap for Int keys (more efficient than Map Int)
type IdMap a = IntMap a

-- IntSet for Int membership
type IdSet = IntSet
```

### When to Use Which

| Container | Use When |
|-----------|----------|
| `HashMap` | Fast lookup, order doesn't matter, hashable keys |
| `Map` | Need ordering, range queries, Ord keys |
| `IntMap` | Int keys, need maximum performance |
| `HashSet` | Fast membership testing, hashable elements |
| `Set` | Need ordering, Ord elements |
| `IntSet` | Int elements, maximum performance |

## Monad Utilities

Relude provides lifted conditional operations.

### Lifted Conditionals

```haskell
-- whenM: Execute action if monadic condition is true
whenM :: Monad m => m Bool -> m () -> m ()

-- Example
processFile :: FilePath -> IO ()
processFile path =
  whenM (doesFileExist path) $ do
    contents <- readFileText path
    processContents contents

-- unlessM: Execute action if monadic condition is false
unlessM :: Monad m => m Bool -> m () -> m ()

-- Example
ensureDirectory :: FilePath -> IO ()
ensureDirectory path =
  unlessM (doesDirectoryExist path) $
    createDirectory path

-- ifM: Monadic if-then-else
ifM :: Monad m => m Bool -> m a -> m a -> m a

-- Example
getConfig :: IO Config
getConfig = ifM (doesFileExist configPath)
  (loadConfig configPath)
  (pure defaultConfig)

-- guardM: MonadPlus guard with monadic condition
guardM :: MonadPlus m => m Bool -> m ()
```

### pass and unit

```haskell
-- pass: Do nothing, return ()
pass :: Applicative f => f ()
pass = pure ()

-- Useful for conditional branches
handleEvent :: Event -> IO ()
handleEvent = \case
  ImportantEvent e -> processImportant e
  IgnorableEvent   -> pass

-- unit: Synonym for ()
unit :: ()
unit = ()
```

### Boolean Operators

```haskell
-- Lifted boolean operators
(&&^) :: Monad m => m Bool -> m Bool -> m Bool
(||^) :: Monad m => m Bool -> m Bool -> m Bool
notM  :: Functor f => f Bool -> f Bool

-- Example
canProceed :: IO Bool
canProceed = hasPermission &&^ isValidState &&^ notM isLocked
```

## Error Handling

### Either Utilities

```haskell
-- Converting between Maybe and Either
maybeToRight :: e -> Maybe a -> Either e a
maybeToRight err Nothing  = Left err
maybeToRight _   (Just x) = Right x

maybeToLeft :: e -> Maybe a -> Either a e
rightToMaybe :: Either e a -> Maybe a
leftToMaybe :: Either e a -> Maybe e

-- Pattern: Validation
data ValidationError = EmptyName | InvalidEmail | TooYoung

validateUser :: UserInput -> Either ValidationError User
validateUser input = do
  name  <- maybeToRight EmptyName (nonEmpty $ inputName input)
  email <- maybeToRight InvalidEmail (parseEmail $ inputEmail input)
  age   <- maybeToRight TooYoung (guardAge $ inputAge input)
  pure User{..}
```

### whenLeft and whenRight

```haskell
-- Execute action on Left value
whenLeft :: Applicative f => Either a b -> (a -> f ()) -> f ()

-- Execute action on Right value
whenRight :: Applicative f => Either a b -> (b -> f ()) -> f ()

-- Example
handleResult :: Either Error Success -> IO ()
handleResult result = do
  whenLeft result $ \err ->
    logError err
  whenRight result $ \success ->
    celebrate success
```

### bug vs error

```haskell
-- error: For expected error conditions
error :: HasCallStack => Text -> a

-- bug: For programmer errors (impossible cases)
bug :: HasCallStack => Text -> a

-- Use bug for cases that should be impossible
processValue :: ValidatedValue -> Result
processValue v = case internalState v of
  Valid x   -> compute x
  Invalid   -> bug "Invalid state after validation - this is a bug"

-- Use error for expected failures
parseConfig :: Text -> Config
parseConfig t = case parse t of
  Left err -> error $ "Invalid config: " <> err
  Right c  -> c
```

## IO Utilities

### Text-Based IO

```haskell
-- Print Text (not String)
putText    :: Text -> IO ()
putTextLn  :: Text -> IO ()
putLText   :: LText -> IO ()
putLTextLn :: LText -> IO ()

-- Read Text from stdin
getLine :: IO Text  -- Returns Text, not String!

-- Example
main :: IO ()
main = do
  putTextLn "Enter your name:"
  name <- getLine  -- Text, not String
  putTextLn $ "Hello, " <> name <> "!"
```

### File IO

```haskell
-- Read file as Text
readFileText :: FilePath -> IO Text

-- Write Text to file
writeFileText :: FilePath -> Text -> IO ()

-- Lazy versions
readFileLText :: FilePath -> IO LText
writeFileLText :: FilePath -> LText -> IO ()

-- Example
processConfigFile :: FilePath -> IO ()
processConfigFile path = do
  content <- readFileText path
  let processed = transform content
  writeFileText (path <> ".processed") processed
```

### ByteString IO

```haskell
-- Read file as ByteString
readFileBS :: FilePath -> IO ByteString

-- Write ByteString to file
writeFileBS :: FilePath -> ByteString -> IO ()

-- Lazy versions
readFileLBS :: FilePath -> IO LByteString
writeFileLBS :: FilePath -> LByteString -> IO ()
```

## Debugging

### Trace Functions

```haskell
-- Show value and return it unchanged
traceShowId :: Show a => a -> a
traceShowId x = traceShow x x

-- Example: Debug a pipeline
result = input
  & transform
  & traceShowId  -- Prints intermediate value
  & finalize

-- Trace with custom message
traceShow :: Show a => a -> b -> b
trace :: Text -> a -> a

-- Example
debugCompute :: Int -> Int
debugCompute x =
  let y = x * 2
  in trace ("Intermediate: " <> show y) (y + 1)
```

### Monadic Trace

```haskell
-- Trace in monadic context
traceM :: Applicative f => Text -> f ()
traceShowM :: (Show a, Applicative f) => a -> f ()

-- Example
processM :: StateT AppState IO Result
processM = do
  state <- get
  traceShowM state  -- Prints current state
  result <- compute
  traceShowM result
  pure result
```

### Debug Best Practices

```haskell
-- DO: Use for temporary debugging
debugFunction :: Input -> Output
debugFunction input =
  let result = actualComputation input
  in traceShowId result  -- REMOVE BEFORE COMMIT

-- DON'T: Leave trace in production code
-- DON'T: Use trace for logging (use a proper logging library)
```

## Module Organization

### Custom Project Prelude

```haskell
-- MyProject/Prelude.hs
module MyProject.Prelude
  ( module Relude
  , module MyProject.Types
  , module MyProject.Utils
  ) where

import Relude
import MyProject.Types
import MyProject.Utils

-- Can add project-specific utilities here
```

### Using Custom Prelude

```haskell
-- MyProject/Feature.hs
{-# LANGUAGE NoImplicitPrelude #-}
module MyProject.Feature where

import MyProject.Prelude  -- Imports Relude + project utilities
```

### Dealing with Prelude-Based Libraries

```haskell
-- When interfacing with String-based libraries
import qualified SomeLibrary as Lib

-- Wrapper using Relude conventions
wrappedFunction :: Text -> IO Text
wrappedFunction input = do
  result <- Lib.stringFunction (toString input)
  pure (toText result)
```

## Migration from Standard Prelude

### Step 1: Add Relude Dependency

```yaml
# package.yaml
dependencies:
  - relude >= 1.0
```

### Step 2: Enable NoImplicitPrelude

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
module MyModule where

import Relude
```

### Step 3: Fix Compilation Errors

```haskell
-- String -> Text
-- Old
name :: String
name = "hello"

-- New
name :: Text
name = "hello"  -- Works with OverloadedStrings

-- Partial functions -> Safe alternatives
-- Old
firstItem = head items

-- New
firstItem = viaNonEmpty head items  -- Returns Maybe
-- or
firstItem = case nonEmpty items of
  Nothing -> defaultValue
  Just ne -> head ne
```

### Step 4: Update IO Operations

```haskell
-- Old
main = do
  line <- Prelude.getLine  -- Returns String
  putStrLn line

-- New
main = do
  line <- getLine  -- Returns Text
  putTextLn line
```

## Quick Reference

| Prelude | Relude Alternative | Notes |
|---------|-------------------|-------|
| `String` | `Text` | Use `OverloadedStrings` |
| `head` | `viaNonEmpty head` | Returns `Maybe a` |
| `tail` | `viaNonEmpty tail` | Returns `Maybe [a]` |
| `last` | `viaNonEmpty last` | Returns `Maybe a` |
| `init` | `viaNonEmpty init` | Returns `Maybe [a]` |
| `!!` | `!?` | Returns `Maybe a` |
| `fromJust` | `fromMaybe` | Requires default value |
| `read` | `readMaybe` | Returns `Maybe a` |
| `undefined` | `error` / `bug` | Use `bug` for impossible cases |
| `putStrLn` | `putTextLn` | Takes `Text` |
| `getLine` | `getLine` | Returns `Text` |
| `show` | `show` | Returns `Text` |
| `readFile` | `readFileText` | Returns `Text` |
| `writeFile` | `writeFileText` | Takes `Text` |

## Best Practices

### DO

- Use `Text` everywhere for string data
- Use `NonEmpty` to guarantee non-empty collections
- Use `viaNonEmpty` for safe list operations
- Use `maybeToRight`/`rightToMaybe` for conversions
- Use `whenM`/`unlessM` for lifted conditionals
- Use `bug` for impossible cases (programmer errors)
- Use qualified imports for container modules

### DON'T

- Use partial functions (`head`, `tail`, `fromJust`, etc.)
- Convert `Text` to `String` unnecessarily
- Leave `trace` statements in production code
- Use `error` for impossible cases (use `bug`)
- Use `String` for new code

### Pattern: Safe Data Processing

```haskell
-- Good: Type-safe, total functions
processUsers :: [User] -> Either ProcessError Report
processUsers users = do
  neUsers <- maybeToRight NoUsers (nonEmpty users)
  let primary = head neUsers  -- Safe! NonEmpty guarantees
  let others = tail neUsers
  pure $ generateReport primary others

-- Bad: Partial functions, runtime crashes
processUsersBad :: [User] -> Report
processUsersBad users =
  let primary = head users  -- Crashes on empty!
      others = tail users   -- Crashes on empty!
  in generateReport primary others
```

### Pattern: Configuration Loading

```haskell
data ConfigError = FileNotFound | ParseError Text | ValidationError Text

loadConfig :: FilePath -> IO (Either ConfigError Config)
loadConfig path = runExceptT $ do
  exists <- liftIO $ doesFileExist path
  unless exists $ throwError FileNotFound

  content <- liftIO $ readFileText path
  parsed <- ExceptT $ pure $ parseConfig content
  validated <- ExceptT $ pure $ validateConfig parsed
  pure validated

  where
    parseConfig :: Text -> Either ConfigError RawConfig
    parseConfig t = maybeToRight (ParseError "Invalid YAML") (decodeYaml t)

    validateConfig :: RawConfig -> Either ConfigError Config
    validateConfig raw = maybeToRight (ValidationError "Missing required fields") (validate raw)
```
