---
description: Fix Haskell build errors, GHC warnings, and HLint issues incrementally. Invokes the haskell-build-resolver agent for minimal, surgical fixes.
---

# Haskell Build and Fix

This command invokes the **haskell-build-resolver** agent to incrementally fix Haskell build errors with minimal changes.

## What This Command Does

1. **Run Diagnostics**: Execute `stack build`, `cabal build`, or `ghc`
2. **Parse Errors**: Group by file and sort by severity
3. **Fix Incrementally**: One error at a time
4. **Verify Each Fix**: Re-run build after each change
5. **Report Summary**: Show what was fixed and what remains

## When to Use

Use `/haskell-build` when:
- `stack build` fails with errors
- `cabal build` reports issues
- GHC shows type errors or warnings
- `hlint` suggests improvements
- After pulling changes that break the build

## Diagnostic Commands Run

```bash
# Primary build check (Stack)
stack build --fast

# Primary build check (Cabal)
cabal build all

# GHC warnings
stack build --ghc-options="-Wall -Wextra"

# Linting
hlint src/

# Dependency check
stack ls dependencies
cabal outdated
```

## Example Session

```text
User: /haskell-build

Agent:
# Haskell Build Resolution

## Initial Diagnostics

```bash
$ stack build --fast

src/Service/User.hs:25:15: error:
    • Variable not in scope: UserRepository
    • Perhaps you meant 'userRepository' (imported from Data.Repository)

src/Handler/API.hs:42:9: error:
    • Couldn't match expected type 'Int' with actual type 'Text'
    • In the expression: userId

src/Handler/API.hs:58:1: error:
    • Couldn't match expected type 'Handler User'
                  with actual type 'Handler ()'
    • The equation(s) for 'getUser' have one argument,
      but its type 'Handler User' has none
```

Errors found: 3

## Fix 1: Undefined Variable

File: src/Service/User.hs:25
Error: Variable not in scope: UserRepository
Cause: Typo or missing import

```haskell
-- Added import
import Data.Repository (UserRepository)

-- Changed
repo :: UserRepository
-- Verified identifier matches import
```

```bash
$ stack build --fast
# 2 errors remaining
```

## Fix 2: Type Mismatch

File: src/Handler/API.hs:42
Error: Couldn't match expected type 'Int' with actual type 'Text'

```haskell
-- Changed
processUser :: Int -> Handler User
processUser userId = ...
  where count = userId  -- userId is Text, not Int

-- To
processUser :: Text -> Handler User
processUser userIdText = ...
  where userId = readMaybe (T.unpack userIdText) :: Maybe Int
```

```bash
$ stack build --fast
# 1 error remaining
```

## Fix 3: Return Type Mismatch

File: src/Handler/API.hs:58
Error: Couldn't match expected type 'Handler User' with actual type 'Handler ()'

```haskell
-- Changed
getUser :: UserId -> Handler User
getUser uid = do
  mUser <- runDB $ selectFirst [UserId ==. uid] []
  case mUser of
    Nothing -> notFound
    Just user -> pure ()  -- Wrong return type!

-- To
getUser :: UserId -> Handler User
getUser uid = do
  mUser <- runDB $ selectFirst [UserId ==. uid] []
  case mUser of
    Nothing -> notFound
    Just (Entity _ user) -> pure user  -- Return the user
```

```bash
$ stack build --fast
# Build successful!
```

## Final Verification

```bash
$ stack build --fast --ghc-options="-Wall"
# No warnings

$ stack test --fast
All 42 tests passed.
```

## Summary

| Metric | Count |
|--------|-------|
| Build errors fixed | 3 |
| GHC warnings fixed | 0 |
| Files modified | 2 |
| Remaining issues | 0 |

Build Status: ✅ SUCCESS
```

## Common Errors Fixed

| Error | Typical Fix |
|-------|-------------|
| `Variable not in scope` | Add import or fix typo |
| `Couldn't match type 'X' with 'Y'` | Fix type signature or conversion |
| `No instance for (Show X)` | Add deriving or instance |
| `Ambiguous type variable` | Add type annotation |
| `Not in scope: data constructor` | Export constructor or add import |
| `Couldn't match expected type` | Fix return type |
| `Redundant constraint` | Remove unused constraint |
| `Parse error` | Fix syntax |
| `Module not found` | Add dependency or fix module path |

## Common GHC Warnings

| Warning | Typical Fix |
|---------|-------------|
| `-Wunused-imports` | Remove unused import |
| `-Wunused-binds` | Remove or use binding |
| `-Wincomplete-patterns` | Add missing pattern match |
| `-Wname-shadowing` | Rename variable |
| `-Wmissing-signatures` | Add type signature |
| `-Worphans` | Move instance to appropriate module |

## Fix Strategy

1. **Parse errors first** - Code must be syntactically valid
2. **Type errors second** - Code must type-check
3. **GHC warnings third** - Fix incomplete patterns, unused bindings
4. **HLint suggestions fourth** - Style improvements
5. **One fix at a time** - Verify each change
6. **Minimal changes** - Don't refactor, just fix

## Type Error Resolution Tips

```haskell
-- When you see: Couldn't match type 'X' with 'Y'
-- Check: Are you returning the right type?

-- When you see: No instance for (Monad X)
-- Check: Is X actually a Monad? Do you need to derive it?

-- When you see: Ambiguous type variable
-- Add explicit type annotation:
result <- readMaybe str :: Maybe Int

-- When you see: Couldn't match expected type 'a -> b' with actual type 'c'
-- Check: Are you applying too many/few arguments?
```

## Stop Conditions

The agent will stop and report if:
- Same error persists after 3 attempts
- Fix introduces more type errors
- Requires architectural changes
- Missing external dependencies
- Circular dependency detected

## Related Commands

- `/haskell-test` - Run tests after build succeeds
- `/haskell-review` - Review code quality
- `/verify` - Full verification loop

## Related

- Agent: `agents/haskell-build-resolver.md`
- Skill: `skills/haskell-patterns/`
- Skill: `skills/haskell-testing/`
