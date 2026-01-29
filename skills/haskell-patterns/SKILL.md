---
name: haskell-patterns
description: Idiomatic Haskell patterns, best practices, and conventions for building robust, type-safe, and maintainable Haskell applications.
---

# Haskell Development Patterns

Idiomatic Haskell patterns and best practices for building robust, type-safe, and maintainable applications.

## When to Activate

- Writing new Haskell code
- Reviewing Haskell code
- Refactoring existing Haskell code
- Designing Haskell modules/packages

## Core Principles

### 1. Types as Documentation

Haskell's type system is expressive enough to encode invariants. Use types to make illegal states unrepresentable.

```haskell
-- Good: Types encode constraints
newtype Email = Email { unEmail :: Text }
  deriving (Show, Eq)

newtype PositiveInt = PositiveInt { unPositiveInt :: Int }
  deriving (Show, Eq, Ord)

mkPositiveInt :: Int -> Maybe PositiveInt
mkPositiveInt n
  | n > 0     = Just (PositiveInt n)
  | otherwise = Nothing

-- Bad: Primitive types with runtime checks
validateEmail :: String -> Bool
validateEmail = -- runtime check every time
```

### 2. Make Illegal States Unrepresentable

```haskell
-- Bad: Can construct invalid state
data User = User
  { userName  :: String
  , userEmail :: String  -- Could be invalid
  , userAge   :: Int     -- Could be negative
  }

-- Good: Types prevent invalid states
data User = User
  { userName  :: NonEmptyText
  , userEmail :: Email
  , userAge   :: Natural
  }

-- Good: Use sum types for valid states only
data ConnectionState
  = Disconnected
  | Connecting Host Port
  | Connected Socket
  | Disconnecting Socket

-- Bad: Multiple fields that must be in sync
data BadConnection = BadConnection
  { isConnected :: Bool
  , socket      :: Maybe Socket  -- Must match isConnected
  }
```

### 3. Prefer Pure Functions

Keep effects at the edges. Pure functions are easier to test and reason about.

```haskell
-- Good: Pure core, effectful shell
parseConfig :: Text -> Either ParseError Config
parseConfig = -- pure parsing logic

loadConfig :: FilePath -> IO (Either ParseError Config)
loadConfig path = parseConfig <$> readFile path

-- Good: Separate pure logic from IO
processData :: [Record] -> Summary
processData = -- pure transformation

runReport :: IO ()
runReport = do
  records <- fetchRecords
  let summary = processData records  -- Pure!
  printSummary summary

-- Bad: Mixing pure logic with IO
processDataBad :: IO Summary
processDataBad = do
  records <- fetchRecords
  -- Pure logic mixed with IO
  let filtered = filter isValid records
  putStrLn "Processing..."  -- Unnecessary IO
  return $ summarize filtered
```

## Error Handling Patterns

### Either for Expected Errors

```haskell
-- Define domain-specific error types
data ValidationError
  = EmptyField Text
  | InvalidFormat Text Text
  | OutOfRange Text Int Int Int
  deriving (Show, Eq)

data AppError
  = ValidationErr ValidationError
  | DatabaseErr Text
  | NetworkErr Text
  deriving (Show, Eq)

-- Use Either for recoverable errors
validateUser :: UserInput -> Either ValidationError User
validateUser input = do
  name  <- validateName (inputName input)
  email <- validateEmail (inputEmail input)
  age   <- validateAge (inputAge input)
  pure User { userName = name, userEmail = email, userAge = age }

-- Chain validations with Either monad
validateName :: Text -> Either ValidationError NonEmptyText
validateName t
  | T.null t  = Left (EmptyField "name")
  | otherwise = Right (NonEmptyText t)
```

### ExceptT for Effectful Error Handling

```haskell
import Control.Monad.Except

type App a = ExceptT AppError IO a

runApp :: App a -> IO (Either AppError a)
runApp = runExceptT

fetchUser :: UserId -> App User
fetchUser uid = do
  mUser <- liftIO $ queryUser uid
  case mUser of
    Nothing   -> throwError (DatabaseErr "User not found")
    Just user -> pure user

handleAppError :: AppError -> IO ()
handleAppError (ValidationErr e) = putStrLn $ "Validation: " ++ show e
handleAppError (DatabaseErr e)   = putStrLn $ "Database: " ++ show e
handleAppError (NetworkErr e)    = putStrLn $ "Network: " ++ show e
```

### Maybe for Absence

```haskell
-- Good: Maybe for optional values
findUser :: UserId -> Map UserId User -> Maybe User
findUser = Map.lookup

-- Use maybe/fromMaybe for defaults
getUserName :: UserId -> Map UserId User -> Text
getUserName uid users =
  maybe "Unknown" userName (findUser uid users)

-- Chain with Maybe monad
getManagerEmail :: Employee -> Map EmployeeId Employee -> Maybe Email
getManagerEmail emp employees = do
  managerId <- employeeManager emp
  manager   <- Map.lookup managerId employees
  pure (employeeEmail manager)
```

### Never Use Partial Functions

```haskell
-- Bad: Partial functions can crash
head []           -- Exception!
tail []           -- Exception!
fromJust Nothing  -- Exception!
read "not a num"  -- Exception!

-- Good: Total alternatives
import Data.List.NonEmpty (NonEmpty, head, tail)
import Safe (headMay, tailMay, readMay)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- Good: Use pattern matching
processFirst :: [Item] -> Result
processFirst []     = emptyResult
processFirst (x:xs) = process x xs
```

## Type Class Patterns

### Deriving Common Instances

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

-- Use deriving for standard instances
data User = User
  { userName  :: Text
  , userEmail :: Email
  , userAge   :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Newtype deriving for wrapped types
newtype UserId = UserId { unUserId :: UUID }
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON, Hashable)
```

### Define Instances Thoughtfully

```haskell
-- Good: Lawful instances
instance Semigroup Summary where
  Summary a1 b1 <> Summary a2 b2 = Summary (a1 + a2) (b1 + b2)

instance Monoid Summary where
  mempty = Summary 0 0

-- Laws to verify:
-- x <> mempty == x
-- mempty <> x == x
-- (x <> y) <> z == x <> (y <> z)

-- Good: Orphan instance warning
-- Define instances in the same module as the type
-- or use newtype wrapper for orphans
newtype MyWrapper = MyWrapper SomeExternalType

instance MyClass MyWrapper where
  -- ...
```

### Custom Type Classes

```haskell
-- Define focused, minimal type classes
class Identifiable a where
  identifier :: a -> Text

class Timestamped a where
  createdAt :: a -> UTCTime
  updatedAt :: a -> UTCTime

-- Combine with constraints
processAuditable
  :: (Identifiable a, Timestamped a)
  => a -> AuditLog
processAuditable x = AuditLog
  { logId = identifier x
  , logCreated = createdAt x
  , logUpdated = updatedAt x
  }
```

## Monad Transformer Patterns

### Standard Transformer Stack

```haskell
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State

-- Common app monad stack
type App a = ReaderT Config (ExceptT AppError IO) a

runApp :: Config -> App a -> IO (Either AppError a)
runApp cfg app = runExceptT (runReaderT app cfg)

-- Using the stack
getConfigValue :: App Text
getConfigValue = asks configValue

failWithError :: AppError -> App a
failWithError = throwError

doIO :: IO a -> App a
doIO = liftIO
```

### MTL Style

```haskell
-- Constraint-based approach (more flexible)
fetchData
  :: (MonadReader Config m, MonadError AppError m, MonadIO m)
  => UserId
  -> m User
fetchData uid = do
  cfg <- ask
  result <- liftIO $ queryWithConfig cfg uid
  case result of
    Left err   -> throwError (DatabaseErr err)
    Right user -> pure user
```

### Avoid Deep Nesting

```haskell
-- Bad: Deep transformer stacks
type DeepStack a = StateT S (ReaderT R (ExceptT E (WriterT W IO))) a

-- Good: Flatten or use effect systems
-- Option 1: Custom monad with records
data AppEnv = AppEnv
  { appConfig :: Config
  , appState  :: IORef AppState
  , appLogger :: Logger
  }

newtype App a = App { unApp :: ReaderT AppEnv IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)

-- Option 2: Use effect systems (Polysemy, Effectful, etc.)
```

## Lens Patterns

### Basic Lens Usage

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

data Address = Address
  { _street :: Text
  , _city   :: Text
  , _zip    :: Text
  } deriving (Show, Eq)

data Person = Person
  { _name    :: Text
  , _age     :: Int
  , _address :: Address
  } deriving (Show, Eq)

makeLenses ''Address
makeLenses ''Person

-- View (get)
getName :: Person -> Text
getName p = p ^. name

-- Set
setAge :: Int -> Person -> Person
setAge n p = p & age .~ n

-- Modify
incrementAge :: Person -> Person
incrementAge p = p & age %~ (+1)

-- Nested access
getCity :: Person -> Text
getCity p = p ^. address . city

-- Nested update
updateCity :: Text -> Person -> Person
updateCity c p = p & address . city .~ c
```

### Prism for Sum Types

```haskell
data Result
  = Success Value
  | Failure Error
  deriving (Show, Eq)

makePrisms ''Result

-- Preview (partial get)
getValue :: Result -> Maybe Value
getValue r = r ^? _Success

-- Review (construct)
mkSuccess :: Value -> Result
mkSuccess = review _Success

-- Modify if present
mapSuccess :: (Value -> Value) -> Result -> Result
mapSuccess f r = r & _Success %~ f
```

## Concurrency Patterns

### STM for Shared State

```haskell
import Control.Concurrent.STM

data Counter = Counter { unCounter :: TVar Int }

newCounter :: IO Counter
newCounter = Counter <$> newTVarIO 0

increment :: Counter -> STM ()
increment (Counter tv) = modifyTVar' tv (+1)

getCount :: Counter -> STM Int
getCount (Counter tv) = readTVar tv

-- Atomic transactions
transfer :: TVar Int -> TVar Int -> Int -> STM ()
transfer from to amount = do
  fromBalance <- readTVar from
  when (fromBalance < amount) retry
  modifyTVar' from (subtract amount)
  modifyTVar' to (+ amount)
```

### Async for Concurrent Operations

```haskell
import Control.Concurrent.Async

-- Run concurrently
fetchBoth :: IO (User, Posts)
fetchBoth = concurrently fetchUser fetchPosts

-- Race (first to complete wins)
fetchWithTimeout :: Int -> IO a -> IO (Maybe a)
fetchWithTimeout micros action =
  either (const Nothing) Just <$>
    race (threadDelay micros) action

-- Map concurrently
fetchAllUsers :: [UserId] -> IO [User]
fetchAllUsers = mapConcurrently fetchUser

-- Structured concurrency with bracket
withWorkers :: Int -> (WorkerPool -> IO a) -> IO a
withWorkers n action = do
  pool <- newWorkerPool n
  action pool `finally` shutdownPool pool
```

### Resource Management

```haskell
import Control.Exception (bracket)

-- Bracket pattern for resource safety
withFile :: FilePath -> (Handle -> IO a) -> IO a
withFile path = bracket (openFile path ReadMode) hClose

-- ResourceT for multiple resources
import Control.Monad.Trans.Resource

withResources :: ResourceT IO ()
withResources = do
  (_, h1) <- allocate (openFile "a.txt" ReadMode) hClose
  (_, h2) <- allocate (openFile "b.txt" WriteMode) hClose
  -- Both handles closed on exit, even with exceptions
  liftIO $ copyContents h1 h2
```

## Module Organization

### Standard Project Layout

```text
my-project/
├── app/
│   └── Main.hs              # Entry point
├── src/
│   └── MyProject/
│       ├── Types.hs         # Core types
│       ├── Config.hs        # Configuration
│       ├── Database.hs      # Database access
│       ├── API.hs           # API handlers
│       └── Internal/        # Internal modules
│           └── Utils.hs
├── test/
│   └── MyProject/
│       ├── TypesSpec.hs
│       └── DatabaseSpec.hs
├── my-project.cabal
├── stack.yaml
└── README.md
```

### Export Lists

```haskell
-- Good: Explicit exports
module MyProject.User
  ( -- * Types
    User(..)
  , UserId
  , mkUserId
    -- * Operations
  , createUser
  , findUser
  , updateUser
    -- * Validation
  , validateUser
  ) where

-- Internal module (not exported from library)
module MyProject.Internal.Database
  ( -- Only export what's needed
    runQuery
  , QueryResult
  ) where
```

### Smart Constructors

```haskell
module MyProject.Email
  ( Email  -- Type only, not constructor
  , mkEmail
  , unEmail
  , emailToText
  ) where

newtype Email = Email Text
  deriving (Show, Eq)

-- Smart constructor validates
mkEmail :: Text -> Either ValidationError Email
mkEmail t
  | isValidEmail t = Right (Email t)
  | otherwise      = Left (InvalidFormat "email" t)

-- Safe unwrap
unEmail :: Email -> Text
unEmail (Email t) = t
```

## Performance Patterns

### Strictness Annotations

```haskell
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}  -- Make all fields strict by default

-- Explicit strict fields
data Stats = Stats
  { !count :: !Int
  , !total :: !Double
  }

-- Bang patterns for strict evaluation
sumStrict :: [Int] -> Int
sumStrict = go 0
  where
    go !acc []     = acc
    go !acc (x:xs) = go (acc + x) xs

-- Avoid lazy accumulator
-- Bad: builds thunks
foldl (+) 0 [1..1000000]

-- Good: strict fold
foldl' (+) 0 [1..1000000]
```

### Text vs String

```haskell
-- Bad: String is [Char], very inefficient
processString :: String -> String
processString = map toUpper

-- Good: Use Text for unicode text
import qualified Data.Text as T

processText :: Text -> Text
processText = T.toUpper

-- Good: Use ByteString for binary data
import qualified Data.ByteString as BS

readBinary :: FilePath -> IO ByteString
readBinary = BS.readFile
```

### Efficient Data Structures

```haskell
-- Use appropriate containers
import qualified Data.Map.Strict as Map      -- Balanced tree, O(log n)
import qualified Data.HashMap.Strict as HM   -- Hash map, O(1) average
import qualified Data.Set as Set             -- Ordered set
import qualified Data.HashSet as HS          -- Hash set
import qualified Data.IntMap as IM           -- Specialized for Int keys
import qualified Data.Vector as V            -- Efficient arrays
import qualified Data.Sequence as Seq        -- Efficient deque

-- Choose based on access pattern
-- Map: ordered keys, range queries
-- HashMap: fast lookup, unordered
-- IntMap: Int keys (very fast)
-- Vector: indexed access, bulk operations
-- Sequence: efficient both ends
```

## Common Extensions

```haskell
{-# LANGUAGE OverloadedStrings #-}    -- String literals as Text
{-# LANGUAGE LambdaCase #-}           -- \case syntax
{-# LANGUAGE RecordWildCards #-}      -- Record puns
{-# LANGUAGE NamedFieldPuns #-}       -- Partial record puns
{-# LANGUAGE TupleSections #-}        -- (,x) as \a -> (a,x)
{-# LANGUAGE TypeApplications #-}     -- read @Int "42"
{-# LANGUAGE DerivingStrategies #-}   -- Explicit deriving
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
```

## Quick Reference: Haskell Idioms

| Idiom | Description |
|-------|-------------|
| Types over tests | Encode invariants in types |
| Parse, don't validate | Convert unstructured to structured data once |
| Make illegal states unrepresentable | Use sum types and newtypes |
| Referential transparency | Same input, same output |
| Composition over inheritance | Small functions composed together |
| Effects at the edges | Pure core, effectful shell |
| Total functions | Handle all cases, no exceptions |
| Explicit over implicit | Prefer explicit type signatures |

## Anti-Patterns to Avoid

```haskell
-- Bad: Partial functions
head, tail, init, last  -- Use NonEmpty or pattern match
fromJust               -- Use pattern match or maybe
read                   -- Use readMaybe

-- Bad: Lazy IO
readFile               -- Use strict variants or streaming

-- Bad: Orphan instances
instance Show SomeExternalType where  -- Define in same module as type

-- Bad: String for text
processData :: String -> String  -- Use Text

-- Bad: Overly generic signatures
f :: a -> a  -- Too generic if you can be more specific

-- Bad: Large tuples
type Result = (Int, String, Bool, Maybe Int, [String])
-- Use records instead

-- Bad: Boolean blindness
process :: Bool -> Bool -> Data -> Result
-- Use newtypes: newtype Verbose = Verbose Bool
```

**Remember**: In Haskell, if it compiles, it often works. Invest time in getting the types right, and the implementation often follows naturally.
