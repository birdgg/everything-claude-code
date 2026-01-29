---
description: Comprehensive Haskell code review for idiomatic patterns, type safety, purity, and performance. Invokes the haskell-reviewer agent.
---

# Haskell Code Review

This command invokes the **haskell-reviewer** agent for comprehensive Haskell-specific code review.

## What This Command Does

1. **Identify Haskell Changes**: Find modified `.hs` files via `git diff`
2. **Run Static Analysis**: Execute `hlint`, `weeder`, and check GHC warnings
3. **Type Safety Review**: Check for partial functions, incomplete patterns
4. **Purity Check**: Analyze use of unsafe functions and IO in pure code
5. **Performance Review**: Detect space leaks, lazy evaluation issues
6. **Generate Report**: Categorize issues by severity

## When to Use

Use `/haskell-review` when:
- After writing or modifying Haskell code
- Before committing Haskell changes
- Reviewing pull requests with Haskell code
- Onboarding to a new Haskell codebase
- Learning idiomatic Haskell patterns

## Review Categories

### CRITICAL (Must Fix)
- Partial function usage (`head`, `tail`, `fromJust`, `read`)
- Incomplete pattern matches
- SQL/Command injection vulnerabilities
- Use of `unsafePerformIO` without justification
- Hardcoded credentials
- Exceptions in pure code (`error`, `undefined`)

### HIGH (Should Fix)
- Race conditions without STM/MVar
- Thread leaks (spawned without cleanup)
- Missing error handling in IO
- Orphan instances
- Global mutable state (top-level IORefs)
- Deadlock-prone MVar usage

### MEDIUM (Consider)
- Space leaks from lazy evaluation
- Using `String` instead of `Text`/`ByteString`
- Missing Haddock comments on exports
- Non-idiomatic code patterns
- Point-free abuse (unreadable composition)
- Missing strictness annotations

### Relude-Specific (If Using Relude)
- Not using `viaNonEmpty` for safe list operations
- Using base `show` instead of Relude's Text-based `show`
- Importing from base when Relude exports the same
- Not using `whenM`/`unlessM` for monadic conditions
- Using `String` when `Text` is the Relude default

## Automated Checks Run

```bash
# Linting
hlint .

# Formatting check
fourmolu --mode check .

# Build with warnings
cabal build --ghc-options="-Wall -Werror"
# or
stack build --ghc-options="-Wall -Werror"
```

## Example Usage

```text
User: /haskell-review

Agent:
# Haskell Code Review Report

## Files Reviewed
- src/User.hs (modified)
- src/Auth.hs (modified)

## Static Analysis Results
✓ hlint: 2 suggestions
✓ weeder: No dead code

## Issues Found

[CRITICAL] Partial Function Usage
File: src/User.hs:45
Issue: Using `head` on potentially empty list
```haskell
users = head results  -- Crashes on empty list!
```
Fix: Use safe alternative
```haskell
users = listToMaybe results
-- or pattern match
users = case results of
  (x:_) -> Just x
  []    -> Nothing
```

[HIGH] Race Condition
File: src/Auth.hs:28
Issue: IORef modified without atomicity
```haskell
modifyIORef sessionCache (Map.insert sid session)
```
Fix: Use atomicModifyIORef' or STM
```haskell
atomicModifyIORef' sessionCache $ \m ->
  (Map.insert sid session m, ())
-- Better: Use TVar
atomically $ modifyTVar' sessionCache (Map.insert sid session)
```

[MEDIUM] Space Leak
File: src/User.hs:62
Issue: Using lazy foldl
```haskell
total = foldl (+) 0 values
```
Fix: Use strict foldl'
```haskell
total = foldl' (+) 0 values
```

[MEDIUM] Relude: Not Using Safe Alternatives
File: src/User.hs:15
Issue: Using base `head` instead of Relude's safe version
```haskell
firstUser = head users
```
Fix: Use viaNonEmpty
```haskell
firstUser = viaNonEmpty head users  -- Returns Maybe User
```

## Summary
- CRITICAL: 1
- HIGH: 1
- MEDIUM: 1

Recommendation: ❌ Block merge until CRITICAL issue is fixed
```

## Approval Criteria

| Status | Condition |
|--------|-----------|
| ✅ Approve | No CRITICAL or HIGH issues |
| ⚠️ Warning | Only MEDIUM issues (merge with caution) |
| ❌ Block | CRITICAL or HIGH issues found |

## Integration with Other Commands

- Use `/haskell-test` first to ensure tests pass
- Use `/haskell-build` if build errors occur
- Use `/haskell-review` before committing
- Use `/code-review` for non-Haskell specific concerns

## Related

- Agent: `agents/haskell-reviewer.md`
- Skills: `skills/haskell-patterns/`, `skills/haskell-testing/`
