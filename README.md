# nix-hell

A shell scripting language for Nix and NixOS. nix-hell is a fork of
[Hell](https://github.com/chrisdone/hell), Chris Done's typed,
Haskell-dialect scripting language, extended with typed primitives
for the Nix store, flakes, derivations, sops/age secrets, systemd,
and NixOS system management.

Status: experimental, single-maintainer, API changes without notice.
Tracks upstream Hell (currently rebased onto 666.20251111, so
records, variants, sum types, case, entailment-based instance
resolution, and the Warp HTTP server are all available).

## What a script looks like

```haskell
#!/usr/bin/env nix-hell
main = do
  r <- Nix.build "nixpkgs#hello"
  case r of
    Either.Left err -> Exit.die ("build failed: " <> err)
    Either.Right path -> do
      Text.putStrLn ("built: " <> StorePath.toText path)
      q <- Nix.queryRequisites path
      reqs <- Either.either Exit.die IO.pure q
      IO.print (List.length reqs)
```

Scripts are a small dialect of Haskell 2010: no type classes or user
instances, no imports, everything qualified, `main` required. Records,
sum types, case, do-notation, and closures work. See `examples/` for
the full tour; every example doubles as a test.

## Design principles

These are the rules this fork holds itself to. Where the code
violates them, that is a bug.

**Failure is a value.** The guest language has no exceptions, so
every primitive that talks to a process or the filesystem returns
`Either Text a`. A failed `nix build`, an unreachable cache, or a
malformed flake.lock is a `Left` your script can match on, never a
dead interpreter. `Left` messages carry the command, exit code, and
stderr.

**Newtypes carry checked invariants or they don't exist.**
`StorePath.fromText` validates the store prefix, the 32-character
nix-base32 hash, and the name; it returns `Maybe StorePath`.
`Derivation` is distinct from `StorePath` so a `.drv` and a built
output cannot be confused. `Shell.which` returns `Maybe Text`, not a
`StorePath`, because `which` output is only a store path on a pure
PATH and the type must not claim more than was checked.

**Secrets are capabilities, not strings.** `Secret` has no `Show`
instance and cannot be constructed from `Text` in the guest language;
only `Sops.get`, `Sops.getAll`, and `Age.decrypt` produce one.
Plaintext leaves the type through exactly one loudly-named escape
hatch (`Secret.expose`) or through sinks that never surface it as a
value: `Secret.setEnv` (inject into a child process environment) and
`Secret.writeFile` (created 0600 via `openFd`, no
write-then-chmod race).

**Pinned process contracts.** Primitives shell out to `nix`, `sops`,
`age`, `ssh-to-age`, and `systemctl`. The flake wraps the binary with
pinned versions of all of them, so the language's semantics do not
drift with the host. `Process.proc` never invokes a shell;
`Shell.escape` exists only for the legitimate remote case
(constructing a command line for `ssh host "..."`) and is tested by
round-tripping hostile input through a real `sh -c`.

**A small merge surface with upstream.** All NixHell registrations
live in `src/NixHell.hs`, exported as three lists (`nixTypes`,
`nixLits`, `nixInstances`) that `src/Hell.hs` splices in at three
one-line points. Rebasing onto upstream Hell means taking their file
and re-adding four lines. Adding a primitive means editing
`NixHell.hs` only.

## Primitive modules (beyond upstream Hell)

`StorePath`, `Derivation`, `NixHash`, `Flake`, `NixExpr`,
`DerivationSpec`, `FlakeGraph`, `Secret` (types); `Nix.*` (build,
eval, instantiate, store add/copy/sign, requisites, gc roots and
collection, profiles, flake metadata/lock/inputs/update/check/graph,
mkDerivation/realise), `Sops.*`, `Age.*`, `Ssh.toAge`, `Shell.*`,
`NixOS.*` (rebuild, currentSystem, option, generations, rollback),
`Systemd.*` (status, start, stop, restart, logs), `Cache.*`
(persistent KV in `~/.cache/nix-hell`, collision-free keys, atomic
writes).

The generated API docs (`scripts/gen-docs.hell`) list every
primitive, type, and instance with signatures.

## Building

```
nix build            # wrapped binary with pinned runtime deps
nix develop          # GHC 9.10 devshell (cabal, HLS, runtime deps)
```

Note the devshell's `nix-hell` on PATH is the wrapped store binary
baked at `nix develop` entry. During an edit loop, build with cabal
and point the test runner at the fresh binary with `--bin`, or your
edits are not what you are testing.

## Testing

```
nix-hell scripts/test-all.hell
```

Phase 1 typechecks every file in `examples/` concurrently. Phase 2
runs the curated subset with assertions: the negative-build test must
produce `Left` on a bogus attribute, the store-path validator must
reject three malformed shapes, escape must round-trip through `sh`,
the age roundtrip must recover the plaintext and leave a mode-600
file, a realised derivation's output contents must match, and cache
keys that collided under an earlier sanitizer must stay distinct.

Gates for anything needing external state, off by default:
`--slow` (real `nix build`), `--sops` and `--age` (fixtures are
auto-provisioned and decrypt-verified; `secrets/example.yaml`,
`test.pub`, and `/tmp/test.key` are owned by the suite and will be
overwritten), `--sudo`, `--nixos`, `--net`. `--bin PATH` selects the
binary under test.

Known coverage gap, on purpose: destructive primitives
(`NixOS.rebuild`, `NixOS.rollback`, `Nix.gcCollect`,
`Nix.optimiseStore`, `Nix.profileInstall`/`Remove`,
`Nix.flakeUpdate`, `Nix.copy`, `Nix.sign`) are not exercised, because
a test suite that mutates the host system profile is a footgun. The
planned fix is a `--destructive` tier inside the NixOS VM test
framework (`pkgs.nixosTest`), where rebuilds and rollbacks are free.

## sops footgun worth knowing

sops discovers `.sops.yaml` by walking up from the current working
directory, and a discovered config demands a matching creation rule
on encrypt even when `--age` is passed explicitly. If you script
encryption from inside a repo that carries a `.sops.yaml`, either add
a creation rule or set the process working directory outside the
repo. Decryption never consults creation rules. This fork's fixture
provisioning does the CWD sidestep; a future `Sops.encrypt` primitive
must handle it by design.

## Credits and license

Hell is by Chris Done, built on Stephanie Weirich's type-safe
typechecker. Nix extensions by Harry Pray IV. BSD3, same as upstream;
see LICENSE.
