---
name: haskell-effectful-patterns
description: Effectful library usage guide and best practices for building robust Haskell applications with type-safe effect systems. Use when asked to "design effectful" or work with Haskell effectful patterns.
argument-hint: <file-or-pattern>
---

# Haskell Effectful Patterns

Effectful is a performant, easy-to-use extensible effects library for Haskell. It provides type-level effect tracking with seamless ecosystem integration.

## When to Activate

- Building applications with the Effectful library
- Migrating from mtl/transformers to Effectful
- Designing custom effects and handlers
- Working with higher-order effects

## Core Concepts

### The Eff Monad

The `Eff` monad is the core abstraction that tracks effects at the type level:

```haskell
-- Computation with Reader and State effects
example :: (Reader Config :> es, State Counter :> es) => Eff es Int
example = do
  cfg <- ask
  modify (+1)
  gets (*configMultiplier cfg)
```

### The (:>) Constraint

The `(:>)` constraint verifies that an effect exists in the effect stack:

```haskell
-- Effect e must be a member of effect list es
(e :: Effect) :> (es :: [Effect])

-- Example: Function requiring specific effects
processData
  :: (Reader Env :> es, State Cache :> es, Error AppError :> es)
  => Data
  -> Eff es Result
```

### Running Computations

```haskell
-- Pure computation (no effects)
runPureEff :: Eff '[] a -> a

-- Computation with IO
runEff :: Eff '[IOE] a -> IO a

-- Example: Running a full stack
main :: IO ()
main = runEff
     . runReader defaultConfig
     . runStateLocal initialState
     . runError @AppError
     $ myApp
```

## Built-in Effects

### Reader Effect

Provides read-only environment access:

```haskell
import Effectful
import Effectful.Reader.Dynamic

data Config = Config
  { configHost :: String
  , configPort :: Int
  }

getServerUrl :: Reader Config :> es => Eff es String
getServerUrl = do
  cfg <- ask
  pure $ configHost cfg ++ ":" ++ show (configPort cfg)

-- Run with environment
runReader :: r -> Eff (Reader r : es) a -> Eff es a

-- Modify environment locally
local :: Reader r :> es => (r -> r) -> Eff es a -> Eff es a

-- Query environment
ask  :: Reader r :> es => Eff es r
asks :: Reader r :> es => (r -> a) -> Eff es a
```

### State Effect

Provides mutable state:

```haskell
import Effectful
import Effectful.State.Dynamic

data AppState = AppState
  { stateCounter :: Int
  , stateCache   :: Map Text Value
  }

incrementCounter :: State AppState :> es => Eff es ()
incrementCounter = modify $ \s -> s { stateCounter = stateCounter s + 1 }

-- Handlers
runStateLocal  :: s -> Eff (State s : es) a -> Eff es (a, s)
evalStateLocal :: s -> Eff (State s : es) a -> Eff es a
execStateLocal :: s -> Eff (State s : es) a -> Eff es s

-- Shared state (for concurrent access, requires IOE)
runStateShared  :: IOE :> es => s -> Eff (State s : es) a -> Eff es (a, s)
evalStateShared :: IOE :> es => s -> Eff (State s : es) a -> Eff es a
execStateShared :: IOE :> es => s -> Eff (State s : es) a -> Eff es s

-- Operations
get    :: State s :> es => Eff es s
gets   :: State s :> es => (s -> a) -> Eff es a
put    :: State s :> es => s -> Eff es ()
modify :: State s :> es => (s -> s) -> Eff es ()
state  :: State s :> es => (s -> (a, s)) -> Eff es a
```

### Error Effect

Provides typed error handling with call stacks:

```haskell
import Effectful
import Effectful.Error.Dynamic

data AppError
  = NotFound Text
  | ValidationError Text
  | DatabaseError Text
  deriving (Show, Eq)

fetchUser :: (Error AppError :> es, IOE :> es) => UserId -> Eff es User
fetchUser uid = do
  mUser <- liftIO $ queryUser uid
  case mUser of
    Nothing   -> throwError (NotFound $ "User: " <> show uid)
    Just user -> pure user

-- Handler returns Either with CallStack
runError :: Eff (Error e : es) a -> Eff es (Either (CallStack, e) a)

-- Operations
throwError  :: (HasCallStack, Error e :> es) => e -> Eff es a
catchError  :: Error e :> es => Eff es a -> (CallStack -> e -> Eff es a) -> Eff es a
tryError    :: Error e :> es => Eff es a -> Eff es (Either (CallStack, e) a)

-- Example error handling
handleErrors :: Eff '[Error AppError, IOE] a -> IO (Either AppError a)
handleErrors action = runEff $ do
  result <- runError action
  pure $ case result of
    Left (_, err) -> Left err
    Right val     -> Right val
```

### IOE Effect

Provides access to IO operations:

```haskell
import Effectful

-- IOE is the effect for IO operations
readConfig :: IOE :> es => FilePath -> Eff es Config
readConfig path = liftIO $ do
  contents <- readFile path
  pure $ parseConfig contents

-- Running with IOE
runEff :: Eff '[IOE] a -> IO a
```

## Dynamic vs Static Dispatch

Effectful supports two dispatch strategies:

### Static Dispatch (Recommended for Most Cases)

- Resolved at compile time
- Better performance
- Use `Effectful.Reader.Static`, `Effectful.State.Static`, etc.

```haskell
import Effectful.State.Static.Local

-- Static handlers have same API
runStateLocal :: s -> Eff (State s : es) a -> Eff es (a, s)
```

### Dynamic Dispatch

- Resolved at runtime
- Needed for: runtime interpretation changes, MonadReader/MonadState compatibility
- Use `Effectful.Reader.Dynamic`, `Effectful.State.Dynamic`, etc.

```haskell
import Effectful.State.Dynamic

-- Dynamic allows runtime handler switching
-- Useful for testing with different implementations
```

**When to use Dynamic:**
- Need `MonadReader`/`MonadState`/`MonadError` instances
- Changing effect interpretation at runtime
- Writing effect-polymorphic libraries

## Defining Custom Effects

### First-Order Effects (Simple)

Effects that don't use the `m` parameter:

```haskell
{-# LANGUAGE GADTs, TypeFamilies, DataKinds #-}

import Effectful
import Effectful.Dispatch.Dynamic

-- Define effect as GADT
data FileSystem :: Effect where
  ReadFile  :: FilePath -> FileSystem m String
  WriteFile :: FilePath -> String -> FileSystem m ()

-- Boilerplate
type instance DispatchOf FileSystem = Dynamic

-- Smart constructors
readFile' :: FileSystem :> es => FilePath -> Eff es String
readFile' path = send (ReadFile path)

writeFile' :: FileSystem :> es => FilePath -> String -> Eff es ()
writeFile' path contents = send (WriteFile path contents)

-- Real implementation
runFileSystemIO :: IOE :> es => Eff (FileSystem : es) a -> Eff es a
runFileSystemIO = interpret_ $ \case
  ReadFile path        -> liftIO $ readFile path
  WriteFile path contents -> liftIO $ writeFile path contents

-- Mock implementation for testing
runFileSystemPure
  :: Map FilePath String
  -> Eff (FileSystem : es) a
  -> Eff es (a, Map FilePath String)
runFileSystemPure fs = reinterpret_ (runStateLocal fs) $ \case
  ReadFile path -> do
    files <- get
    pure $ fromMaybe "" (Map.lookup path files)
  WriteFile path contents -> do
    modify (Map.insert path contents)
```

### Higher-Order Effects

Effects that take effectful computations as arguments:

```haskell
{-# LANGUAGE GADTs, TypeFamilies, DataKinds #-}

import Effectful
import Effectful.Dispatch.Dynamic

-- Higher-order effect (uses m parameter)
data Transaction :: Effect where
  WithTransaction :: m a -> Transaction m a

type instance DispatchOf Transaction = Dynamic

withTransaction :: Transaction :> es => Eff es a -> Eff es a
withTransaction action = send (WithTransaction action)

-- Handler needs LocalEnv for higher-order operations
runTransaction :: IOE :> es => Eff (Transaction : es) a -> Eff es a
runTransaction = interpret $ \env -> \case
  WithTransaction action -> do
    liftIO $ beginTransaction
    result <- localSeqUnlift env $ \unlift -> unlift action
    liftIO $ commitTransaction
    pure result
```

### Interpretation Functions

```haskell
-- First-order effects (simpler)
interpret_      :: (forall r. e (Eff es) r -> Eff es r) -> Eff (e : es) a -> Eff es a
reinterpret_    :: (Eff handlerEs a -> Eff es a) -> (forall r. e (Eff es) r -> Eff handlerEs r) -> Eff (e : es) a -> Eff es a
interpose_      :: e :> es => (forall r. e (Eff es) r -> Eff es r) -> Eff es a -> Eff es a

-- Higher-order effects (need LocalEnv)
interpret       :: (forall r. LocalEnv localEs es -> e (Eff localEs) r -> Eff es r) -> Eff (e : es) a -> Eff es a
reinterpret     :: (Eff handlerEs a -> Eff es a) -> (forall r. LocalEnv localEs handlerEs -> e (Eff localEs) r -> Eff handlerEs r) -> Eff (e : es) a -> Eff es a
interpose       :: e :> es => (forall r. LocalEnv localEs es -> e (Eff localEs) r -> Eff es r) -> Eff es a -> Eff es a
```

## Higher-Order Effects and Unlifting

### LocalEnv

When interpreting higher-order effects, you need to run `Eff localEs` computations within `Eff es`:

```haskell
interpret :: (forall r. LocalEnv localEs es -> e (Eff localEs) r -> Eff es r)
          -> Eff (e : es) a
          -> Eff es a
```

### Unlifting Strategies

```haskell
-- Sequential unlift (most common)
localSeqUnlift :: LocalEnv localEs es -> ((forall r. Eff localEs r -> Eff es r) -> Eff es a) -> Eff es a

-- Example: Running inner computation
runWithLogging :: Logging :> es => Eff (Transaction : es) a -> Eff es a
runWithLogging = interpret $ \env -> \case
  WithTransaction action -> do
    log "Starting transaction"
    result <- localSeqUnlift env $ \unlift -> do
      unlift action
    log "Committed transaction"
    pure result
```

## Error Handling Patterns

> Source: [effectful discussions #149](https://github.com/haskell-effectful/effectful/discussions/149)

### Pattern 1: Constrained Effect Signatures

When defining effects in effectful, how should operations that can fail be modeled?

Common approaches:
1. Return `Either` - forces callers to manually handle errors
2. Add `Error` to the effect's constraint - pollutes all callers with error handling

**Solution: Use constrained effect signatures** by substituting the effect's `m` parameter with `Eff es` and placing `Error` constraints on specific operations:

```haskell
data FileSystem :: Effect where
  ReadFile :: Error FsReadError :> es => FilePath -> FileSystem (Eff es) String
  WriteFile :: Error FsWriteError :> es => FilePath -> String -> FileSystem (Eff es) ()
```

**Key Insight:** "The caller decides whether to handle, instead of deciding whether to rethrow."

Handler implementation using `localSeqUnlift`:

```haskell
runFileSystemIO ::
  (IOE :> es) =>
  Eff (FileSystem : es) a ->
  Eff es a
runFileSystemIO = interpret $ \env -> \case
  ReadFile path -> do
    result <- liftIO $ try $ readFile path
    case result of
      Left (e :: IOException) ->
        localSeqUnlift env $ \unlift -> unlift $ throwError $ FsReadError (show e)
      Right content -> pure content
```

| Approach | Caller Control | Ergonomics | Effect Scope |
|----------|---------------|------------|--------------|
| Return Either | Manual unwrapping | Poor | Clean |
| Global Error constraint | Forced handling | Medium | Polluted |
| **Constrained signatures** | Optional handling | Good | Precise |

### Pattern 2: liftEither with Error.Static

`liftEither` from `Control.Monad.Except` requires `MonadError` instance, but `Effectful.Error.Static` does NOT provide this instance. Only `Effectful.Error.Dynamic` does.

**Solution:** Create a local `liftEither` helper:

```haskell
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)

liftEither :: Error e :> es => Either e a -> Eff es a
liftEither = either throwError pure
```

Usage:

```haskell
import Data.Bifunctor (first)

-- Convert String errors to Text
result <- liftEither $ first T.pack $ eitherDecode body

-- Convert to custom error type
result <- liftEither $ first MyError $ parseResult
```

### Pattern 3: Catching Errors at Boundaries

**runErrorNoCallStack** - Use when you need `Either` at a specific point:

```haskell
-- Let errors propagate
items <- fetchRss url

-- Or catch at boundary
result <- runErrorNoCallStack $ fetchRss url
case result of
  Left err -> handleError err
  Right items -> processItems items
```

**runErrorWith** - Use for recovery or logging:

```haskell
runErrorWith handler $ do
  items <- fetchRss url
  processItems items
  where
    handler _callstack err = do
      logError err
      pure defaultValue
```

### Error Handling Anti-patterns

1. **Don't** add `Error` constraint to the interpreter instead of the effect:
   ```haskell
   -- Bad: forces all callers to handle errors
   runFS :: (Error FsError :> es) => Eff (FileSystem : es) a -> Eff es a
   ```

2. **Don't** return raw `Either` when errors should propagate:
   ```haskell
   -- Bad: forces manual Either handling everywhere
   data FileSystem :: Effect where
     ReadFile :: FilePath -> FileSystem m (Either FsError String)
   ```

3. **Don't** use `Error.Dynamic` just for `liftEither`:
   ```haskell
   -- Bad: mixing Static and Dynamic unnecessarily
   import Effectful.Error.Dynamic (liftEither)
   ```

### When to Use Each Error Pattern

| Scenario | Pattern |
|----------|---------|
| Defining new effects | Constrained signatures |
| Converting `Either` results | `liftEither` helper |
| Boundary error handling | `runErrorNoCallStack` |
| Error recovery/logging | `runErrorWith` |
| IO exceptions in handlers | `localSeqUnlift` + `throwError` |

## Best Practices

### 1. Prefer Static Dispatch

Use static variants unless you specifically need dynamic features:

```haskell
-- Prefer this
import Effectful.Reader.Static
import Effectful.State.Static.Local

-- Unless you need MonadReader/MonadState instances
import Effectful.Reader.Dynamic
import Effectful.State.Dynamic
```

### 2. Order Effects Consistently

Establish a consistent effect ordering convention:

```haskell
-- Recommended order: Reader, State, Error, IO
type App = Eff '[Reader Config, State AppState, Error AppError, IOE]

runApp :: Config -> AppState -> App a -> IO (Either AppError (a, AppState))
runApp cfg st = runEff
              . runError
              . runStateLocal st
              . runReader cfg
```

### 3. Use Type Aliases for Complex Stacks

```haskell
type AppEffects = '[Reader Config, State Cache, Error AppError, IOE]

type App a = Eff AppEffects a

-- Or with constraints
type AppM es a = (Reader Config :> es, State Cache :> es, IOE :> es) => Eff es a
```

### 4. Keep Effects Minimal

Request only the effects you need:

```haskell
-- Good: Minimal constraints
getPort :: Reader Config :> es => Eff es Int
getPort = asks configPort

-- Bad: Over-constrained
getPort :: (Reader Config :> es, State s :> es, IOE :> es) => Eff es Int
getPort = asks configPort
```

### 5. Use Local/Shared State Appropriately

```haskell
-- Local state: isolated to current thread
runStateLocal :: s -> Eff (State s : es) a -> Eff es (a, s)

-- Shared state: thread-safe, concurrent access
runStateShared :: s -> Eff (State s : es) a -> Eff es (a, s)
```

## Common Patterns

### Dependency Injection

```haskell
data Database :: Effect where
  Query :: Text -> Database m [Row]
  Execute :: Text -> Database m ()

type instance DispatchOf Database = Dynamic

-- Production implementation
runDatabasePostgres :: IOE :> es => Connection -> Eff (Database : es) a -> Eff es a
runDatabasePostgres conn = interpret_ $ \case
  Query sql    -> liftIO $ pgQuery conn sql
  Execute sql  -> liftIO $ pgExecute conn sql

-- Test implementation
runDatabaseMock :: State MockDB :> es => Eff (Database : es) a -> Eff es a
runDatabaseMock = interpret_ $ \case
  Query sql    -> gets (lookupMock sql)
  Execute sql  -> modify (recordExecution sql)
```

### Testing with Mocks

```haskell
spec :: Spec
spec = describe "UserService" $ do
  it "creates users" $ do
    let mockDB = MockDB { ... }
    result <- runPureEff
            . runStateLocal mockDB
            . runDatabaseMock
            . runError @AppError
            $ createUser "alice"
    result `shouldBe` Right (User "alice")
```

### Combining Multiple Effects

```haskell
app :: (Reader Config :> es, State AppState :> es, Error AppError :> es, IOE :> es)
    => Eff es ()
app = do
  cfg <- ask
  when (not $ configValid cfg) $
    throwError (ValidationError "Invalid config")

  modify $ \s -> s { lastRun = getCurrentTime }

  liftIO $ putStrLn "Running..."

runFullApp :: Config -> AppState -> IO (Either (CallStack, AppError) ((), AppState))
runFullApp cfg st = runEff
                  . runError
                  . runStateLocal st
                  . runReader cfg
                  $ app
```

### Wrapping Existing Libraries

```haskell
-- Wrap servant-server handlers
data Handler :: Effect where
  ThrowServant :: ServerError -> Handler m a

type instance DispatchOf Handler = Dynamic

runHandler :: Eff (Handler : es) a -> Eff es (Either ServerError a)
runHandler = reinterpret_ runError $ \case
  ThrowServant err -> throwError err
```

## Migration from MTL

### Before (MTL)

```haskell
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

type App a = ReaderT Config (StateT AppState (ExceptT AppError IO)) a

runApp :: Config -> AppState -> App a -> IO (Either AppError (a, AppState))
runApp cfg st = runExceptT . flip runStateT st . flip runReaderT cfg
```

### After (Effectful)

```haskell
import Effectful
import Effectful.Reader.Dynamic
import Effectful.State.Dynamic
import Effectful.Error.Dynamic

type App a = Eff '[Reader Config, State AppState, Error AppError, IOE] a

runApp :: Config -> AppState -> App a -> IO (Either (CallStack, AppError) (a, AppState))
runApp cfg st = runEff
              . runError
              . runStateLocal st
              . runReader cfg
```

## Performance Considerations

1. **Static dispatch is faster** - Use static variants when possible
2. **Effect order matters** - Effects used together should be adjacent
3. **No INLINE pragmas needed** - Effectful optimizes well at default levels
4. **Avoid deep nesting** - Flatten effect stacks when practical

## Quick Reference

| Operation | Function |
|-----------|----------|
| Run pure | `runPureEff` |
| Run with IO | `runEff` |
| Read environment | `ask`, `asks` |
| Modify environment locally | `local` |
| Get state | `get`, `gets` |
| Set state | `put` |
| Modify state | `modify`, `state` |
| Throw error | `throwError` |
| Catch error | `catchError`, `tryError` |
| Lift IO | `liftIO` |
| Send effect operation | `send` |
| Interpret first-order | `interpret_` |
| Interpret higher-order | `interpret` |
| Reinterpret with private effects | `reinterpret_`, `reinterpret` |

## Further Reading

- [Hackage: effectful](https://hackage.haskell.org/package/effectful)
- [Hackage: effectful-core](https://hackage.haskell.org/package/effectful-core)
- [GitHub: haskell-effectful/effectful](https://github.com/haskell-effectful/effectful)
