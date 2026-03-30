# Nix-Hell: A Typed Shell for the Nix Ecosystem 😈

Nix-Hell is a fork of [Hell](https://github.com/chrisdone/hell), a shell scripting language implemented as a tiny dialect of Haskell, expanded into a domain-specific scripting language for the Nix ecosystem.

## Background

I have been writing shell scripts for as long as I have been writing software, and I have never liked it. Bash is remarkable in the sense that it has survived decades of abuse and still mostly works, but let us be honest: it is a terrible programming language. The quoting rules are a disaster. The error handling is opt-in and routinely forgotten. The type system is "everything is a string, good luck."

This bothers me more than usual when working in Nix. We have this extraordinary system for reproducible, typed, compositional software construction, and then right at the edges of it, where the actual work gets done, we are back to bash scripts. The Nix language is purely functional and quite elegant. Builds are hermetic, the dependency graph is tracked with mathematical precision. And then you write your deployment scripts and you are back in 1989.

A few days ago I found Chris Done's Hell: take the Haskell type checker, strip out everything that makes Haskell complicated, and expose the result as a scripting language. The entire implementation fits in one file. You write do-notation, call processes, manipulate files, and the type checker tells you when you have made a mistake before you run the script on your production database.

So I forked Hell and called it Nix-Hell. The core language stays exactly as Chris designed it. What I added is a layer of Nix-specific primitives and types: a `StorePath` type that the checker distinguishes from plain text, a `Secret` type that cannot accidentally flow into a log message, and first-class primitives for store operations, flake evaluation, derivation construction, and system management.

The great insight borrowed from Hell is that scripting languages do not need to be powerful. They need to be correct. A language that distinguishes `StorePath` from `Text` and `Secret` from `Text` is already dramatically safer than bash without requiring anything more sophisticated than a simply-typed lambda calculus.

## Quick Start

```
# Run a script
nix-hell my-script.hell

# Type-check without running
nix-hell --check my-script.hell

# Shebangs work
#!/usr/bin/env nix-hell
```

A minimal script:

```haskell
main = do
  path <- Nix.build "nixpkgs#hello"
  Text.putStrLn ("built: " <> StorePath.toText path)
```

## Opaque Types

Nix-Hell's core contribution over vanilla Hell is a set of opaque types that the type checker enforces at script-check time.

### StorePath

A validated path under `/nix/store/`. The only way to construct one is through a trusted primitive. Scripts cannot coerce raw `Text` to `StorePath` directly.

```haskell
-- Construction
path <- Nix.build "nixpkgs#hello"           -- IO StorePath
case StorePath.fromText someText of          -- Maybe StorePath
  Maybe.Just p -> ...
  Maybe.Nothing -> ...

-- Extraction (one-way out)
let t = StorePath.toText path               -- Text
```

### Secret

An opaque value with no `Show` instance. It cannot flow into `Text.putStrLn`, string concatenation, or log output. The only extraction points are `Secret.toEnvValue` and `Secret.writeFile`, both of which have names that make every extraction visible in code review.

```haskell
secret <- Sops.get "secrets/prod.yaml" "db_password"
-- Text.putStrLn (Secret.toEnvValue secret)  -- type error if you try to show it
code <- Process.runProcess
  (Process.setEnv [("DB_PASSWORD", Secret.toEnvValue secret)]
    (Process.proc "myapp" []))
```

### NixHash

A content hash in SRI format (`sha256-...`). Only constructible by hashing primitives, never from raw strings.

```haskell
hash <- NixHash.sha256Path path      -- IO NixHash  (works on files or directories)
hash <- NixHash.sha256Text "content" -- IO NixHash
let t = NixHash.toText hash          -- Text
```

### Derivation

An unbuilt `.drv` path. Distinct from `StorePath` so the type checker prevents passing a derivation where a built output is expected and vice-versa.

```haskell
drv <- Nix.instantiate "(import <nixpkgs> {}).hello"  -- IO Derivation
drv <- Nix.mkDerivation spec                          -- IO Derivation

let sp = Derivation.toStorePath drv                   -- StorePath
case Derivation.fromStorePath sp of                   -- Maybe Derivation
  Maybe.Just d -> ...
  Maybe.Nothing -> ...   -- non-.drv path rejected
```

### Flake

A validated flake reference. Prevents raw strings or store paths from being passed where a flake ref is expected.

```haskell
case Flake.fromText "github:NixOS/nixpkgs" of
  Maybe.Just f  -> Text.putStrLn (Flake.toText f)
  Maybe.Nothing -> Exit.die "invalid ref"
```

### NixExpr

A typed Nix expression. Build it with constructors, serialize with `NixExpr.toText`, evaluate with `NixExpr.eval`. The type checker prevents producing malformed Nix.

```haskell
let expr = NixExpr.attrs
      [ ("name",    NixExpr.str "my-package")
      , ("version", NixExpr.str "1.0")
      , ("count",   NixExpr.int 42)
      , ("enable",  NixExpr.true)
      ]
Text.putStrLn (NixExpr.toText expr)
-- { name = "my-package"; version = "1.0"; count = 42; enable = true; }

result <- NixExpr.eval expr   -- IO Value (JSON)
```

Available constructors: `NixExpr.str`, `NixExpr.int`, `NixExpr.bool`, `NixExpr.true`, `NixExpr.false`, `NixExpr.null`, `NixExpr.list`, `NixExpr.attrs`, `NixExpr.path`.

### DerivationSpec

A typed specification for a Nix derivation. Eliminates the class of bugs where derivation attributes are passed as unvalidated strings.

```haskell
bash <- Nix.build "nixpkgs#bash.out"
coreutils <- Nix.build "nixpkgs#coreutils.out"

let bashBin = StorePath.toText bash <> "/bin/bash"
case StorePath.fromText bashBin of
  Maybe.Nothing -> Exit.die "bad path"
  Maybe.Just builder -> do
    let spec = DerivationSpec.make
          "my-derivation"
          builder
          "x86_64-linux"
          ["-c", "echo hello > $out"]
          (Map.fromList [("PATH", StorePath.toText coreutils <> "/bin")])
          []
    drv     <- Nix.mkDerivation spec   -- IO Derivation
    outputs <- Nix.realise drv         -- IO (Map Text StorePath)
    case Map.lookup "out" outputs of
      Maybe.Just p -> Text.putStrLn (StorePath.toText p)
      Maybe.Nothing -> Exit.die "no output"
```

### FlakeGraph

A parsed dependency graph of a flake's inputs, derived from `flake.lock`.

```haskell
graph <- Nix.flakeGraph "."
let nodes = FlakeGraph.nodes graph   -- [Text]
let edges = FlakeGraph.edges graph   -- [(Text, Text)]
let urls  = FlakeGraph.urls graph    -- Map Text Text

case FlakeGraph.detectCycles graph of
  Maybe.Nothing      -> Text.putStrLn "acyclic"
  Maybe.Just cycle   -> Text.putStrLn ("cycle: " <> Text.intercalate " -> " cycle)
```

## Primitive Reference

### Nix Store

```haskell
Nix.build            :: Text -> IO StorePath
Nix.buildFlakeAttr   :: Text -> Text -> IO StorePath
Nix.storeAdd         :: Text -> ByteString -> IO StorePath
Nix.isInStore        :: Text -> IO Bool
Nix.queryRequisites  :: StorePath -> IO [StorePath]
Nix.copy             :: StorePath -> Text -> IO ()
Nix.sign             :: StorePath -> Text -> IO ()
```

### Nix Eval and Flake

```haskell
Nix.eval             :: Text -> IO Value
Nix.evalFlakeAttr    :: Text -> Text -> IO Value
Nix.instantiate      :: Text -> IO Derivation
Nix.flakeMetadata    :: Text -> IO Value
Nix.flakeUpdate      :: Text -> IO ()
Nix.flakeLock        :: Text -> IO Value
Nix.flakeInputs      :: Text -> IO (Map Text Text)
Nix.flakeGraph       :: Text -> IO FlakeGraph
Nix.checkFlakeOutputs :: Text -> IO (Either Text Text)
```

### Derivation Construction

```haskell
DerivationSpec.make  :: Text -> StorePath -> Text -> [Text] -> Map Text Text -> [Text] -> DerivationSpec
Nix.mkDerivation     :: DerivationSpec -> IO Derivation
Nix.realise          :: Derivation -> IO (Map Text StorePath)
```

### Profile and GC

```haskell
Nix.profileInstall   :: StorePath -> IO ()
Nix.profileRemove    :: Text -> IO ()
Nix.profileList      :: IO [StorePath]
Nix.gcCollect        :: IO ()
Nix.gcRoots          :: IO [StorePath]
Nix.addRoot          :: StorePath -> Text -> IO ()
Nix.optimiseStore    :: IO ()
```

### Secrets

```haskell
Sops.get             :: Text -> Text -> IO Secret
Sops.getAll          :: Text -> IO (Map Text Secret)
Secret.toEnvValue    :: Secret -> Text
Secret.writeFile     :: Text -> Secret -> IO ()
```

### Age Encryption

```haskell
Age.encrypt          :: Text -> Text -> IO ByteString   -- pubkey -> plaintext -> ciphertext
Age.decrypt          :: Text -> ByteString -> IO Secret  -- identity file -> ciphertext -> secret
Ssh.toAge            :: Text -> IO Text                  -- ssh pubkey path -> age pubkey
```

### Shell Safety

```haskell
Shell.escape         :: Text -> Text
Shell.escapeList     :: [Text] -> Text
Shell.which          :: Text -> IO (Maybe StorePath)
Shell.inPath         :: Text -> IO Bool
```

### NixOS

```haskell
NixOS.rebuild        :: Text -> IO ExitCode   -- "switch", "test", "boot", "dry-run"
NixOS.currentSystem  :: IO StorePath
NixOS.option         :: Text -> IO Value
NixOS.generations    :: IO Text
NixOS.rollback       :: IO ()
```

### Systemd

```haskell
Systemd.status       :: Text -> IO Text
Systemd.start        :: Text -> IO ExitCode
Systemd.stop         :: Text -> IO ExitCode
Systemd.restart      :: Text -> IO ExitCode
Systemd.logs         :: Text -> IO Text
```

### Persistent Cache

Scripts that call `nix build` or other slow operations can cache results across invocations in `~/.cache/nix-hell/`.

```haskell
Cache.get            :: Text -> IO (Maybe Text)
Cache.set            :: Text -> Text -> IO ()
Cache.getOrRun       :: Text -> IO Text -> IO Text
Cache.invalidate     :: Text -> IO ()
```

Example:

```haskell
path <- Cache.getOrRun "my-build" do
  p <- Nix.build "nixpkgs#hello"
  IO.pure (StorePath.toText p)
-- First run: builds and caches. Subsequent runs: returns immediately.
```

## The Hell Language

Nix-Hell inherits the full Hell language. Key points for users coming from Haskell:

**What works:** do-notation, let bindings, lambdas, pattern matching on user-defined sum types and records, type annotations, polymorphism via type classes (Show, Eq, Ord, Functor, Monad, etc.), if-then-else, list syntax, tuple syntax, string/integer/double literals.

**What does not work:** general recursion (cyclic bindings are rejected), imports, type class definitions, most GHC extensions. Hell is intentionally minimal.

**Pattern matching restrictions that catch people out:**

Wildcards `_` are only valid as the top-level pattern in a case alternative for user-defined sum types. They are not valid inside tuple destructuring or as constructor slot patterns:

```haskell
-- WRONG: wildcard in tuple slot
(\(x, _) -> x)

-- RIGHT: named variable
(\(x, unused) -> x)

-- WRONG: wildcard in constructor slot
case result of
  Maybe.Just _ -> ...

-- RIGHT: named variable
case result of
  Maybe.Just val -> ...
```

Case alternatives for primitive types (`Maybe`, `Either`, `Bool`, `ExitCode`, `These`, `Json.Value`) use a different desugaring path than user-defined types. Only `PVar` patterns are accepted in constructor slots for primitive types.

Cons patterns are not supported in case alternatives:

```haskell
-- WRONG
case xs of
  (x:rest) -> ...
  []       -> ...

-- RIGHT: use List.uncons
case List.uncons xs of
  Maybe.Just (x, rest) -> ...
  Maybe.Nothing        -> ...
```

**Tuple sizes** are limited to 2, 3, and 4.

## Examples

The `examples/` directory contains 64 scripts covering the full primitive set. Run them all with:

```
nix-hell examples/run-all.hell examples/
```

Notable examples:

| File | Demonstrates |
|------|-------------|
| `44-nix-build.hell` | `Nix.build`, `Nix.queryRequisites` |
| `52-nixhash.hell` | `NixHash.sha256Path`, `NixHash.sha256Text` |
| `53-derivation.hell` | `Nix.instantiate`, `Derivation` type |
| `60-nixexpr.hell` | `NixExpr` constructors, `NixExpr.eval` |
| `61-mkderivation.hell` | `DerivationSpec.make`, `Nix.mkDerivation`, `Nix.realise` |
| `62-flake-graph.hell` | `Nix.flakeGraph`, `FlakeGraph.detectCycles` |
| `63-cache.hell` | `Cache.getOrRun`, persistent caching |
| `46-secret-usage.hell` | `Sops.get`, `Secret.toEnvValue` |
| `50-age-roundtrip.hell` | `Age.encrypt`, `Age.decrypt` |

## License

Nix-Hell is a fork of [Hell](https://github.com/chrisdone/hell) by Chris Done, used under the BSD 3-Clause License. The original Hell source is copyright Chris Done. Modifications and additions comprising Nix-Hell are copyright Harry Pray IV. See [LICENSE](./LICENSE) for the full text.