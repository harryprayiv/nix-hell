{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | NixHell: typed Nix primitives for the Hell scripting language.
--
-- Design rules:
--
--   1. Anything that talks to an external process or the filesystem
--      returns @Either Text a@. The guest language has no exceptions;
--      Left is the only honest failure channel. Crashing the
--      interpreter with 'error' is forbidden in this module.
--
--   2. Newtypes carry validated invariants or they don't exist.
--      'StorePath' checks the store hash. 'Secret' never gains a Show
--      instance and only leaves the type via explicitly-named escape
--      hatches ('secret_expose') or capability-style sinks
--      ('secret_setEnv', 'secret_writeFile').
--
--   3. The fork's merge surface with upstream Hell is three splice
--      points, fed by 'nixTypes', 'nixLits', and 'nixInstances'.
--      Adding a primitive means editing this file only.

module NixHell
  ( -- Types
    StorePath(..)
  , Secret(..)
  , NixHash(..)
  , Derivation(..)
  , Flake(..)
  , NixExpr(..)
  , DerivationSpec(..)
  , FlakeGraph(..)
    -- Registration lists for Hell.hs (the only integration surface)
  , nixTypes
  , nixLits
  , nixInstances
    -- StorePath
  , storePath_fromText
  , storePath_toText
    -- Secret
  , secret_expose
  , secret_setEnv
  , secret_writeFile
    -- NixHash
  , nixHash_sha256Path
  , nixHash_sha256Text
  , nixHash_toText
    -- Derivation
  , derivation_fromStorePath
  , derivation_toStorePath
    -- Flake
  , flake_fromText
  , flake_toText
    -- NixExpr
  , nixExpr_str
  , nixExpr_int
  , nixExpr_bool
  , nixExpr_true
  , nixExpr_false
  , nixExpr_null
  , nixExpr_list
  , nixExpr_attrs
  , nixExpr_path
  , nixExpr_toText
  , nixExpr_eval
    -- DerivationSpec
  , derivationSpec_make
  , nix_mkDerivation
  , nix_realise
    -- FlakeGraph
  , nix_flakeGraph
  , flakeGraph_nodes
  , flakeGraph_edges
  , flakeGraph_urls
  , flakeGraph_detectCycles
    -- Cache
  , cache_get
  , cache_set
  , cache_getOrRun
  , cache_invalidate
    -- Nix store
  , nix_build
  , nix_buildFlakeAttr
  , nix_storeAdd
  , nix_isInStore
  , nix_queryRequisites
  , nix_copy
  , nix_sign
    -- Nix eval and flake
  , nix_eval
  , nix_evalFlakeAttr
  , nix_instantiate
  , nix_flakeMetadata
  , nix_flakeUpdate
  , nix_flakeLock
  , nix_flakeInputs
  , nix_checkFlakeOutputs
    -- Profile and GC
  , nix_profileInstall
  , nix_profileRemove
  , nix_profileList
  , nix_gcCollect
  , nix_gcRoots
  , nix_addRoot
  , nix_optimiseStore
    -- Sops
  , sops_get
  , sops_getAll
    -- Age
  , age_encrypt
  , age_decrypt
  , ssh_toAge
    -- Shell
  , shell_escape
  , shell_escapeList
  , shell_which
  , shell_inPath
    -- NixOS
  , nixos_rebuild
  , nixos_currentSystem
  , nixos_option
  , nixos_generations
  , nixos_rollback
    -- Systemd
  , systemd_status
  , systemd_start
  , systemd_stop
  , systemd_restart
  , systemd_logs
  ) where

import Control.Exception (IOException, try)
import Control.Monad (guard)
import Data.Aeson (Value)
import qualified Data.Aeson as Json
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as L
import Data.Char (isAlphaNum, isAscii, ord)
import Data.Constraint (Dict (..))
import Data.Dynamic (Dynamic, toDyn)
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Numeric (showHex)
import qualified System.Directory as Dir
import System.Environment (getEnv, getEnvironment)
import qualified System.IO as IO
import qualified System.IO.Temp as Temp
import qualified System.Posix.Files as PosixFiles
import qualified System.Posix.IO as PosixIO
import System.Process.Typed as Process
import Type.Reflection (SomeTypeRep (..), Typeable, typeRep)

--------------------------------------------------------------------------------
-- Types

-- | A validated Nix store path. Opaque: only constructible by
-- trusted primitives or by 'storePath_fromText', which checks the
-- store hash. Scripts cannot forge one from arbitrary Text.
newtype StorePath = StorePath Text
  deriving (Eq, Ord)

instance Show StorePath where
  show (StorePath t) = Text.unpack t

-- | An opaque secret value. No Show instance: secrets cannot flow
-- into display, logging, or string concatenation. Leaving the type
-- requires 'secret_expose' (loud on purpose) or a capability-style
-- sink ('secret_setEnv', 'secret_writeFile').
newtype Secret = Secret Text

-- | A Nix content hash in SRI format (sha256-<base64>). Only
-- constructible via hashing primitives.
newtype NixHash = NixHash Text
  deriving (Eq, Ord)

instance Show NixHash where
  show (NixHash t) = Text.unpack t

-- | An unbuilt derivation (.drv path). Distinct from StorePath so
-- the type checker prevents confusing a derivation with a built output.
newtype Derivation = Derivation StorePath
  deriving (Eq, Ord)

instance Show Derivation where
  show (Derivation (StorePath t)) = Text.unpack t

-- | A validated flake reference.
newtype Flake = Flake Text
  deriving (Eq, Ord)

instance Show Flake where
  show (Flake t) = Text.unpack t

-- | A typed Nix expression. Build with NixExpr.* constructors;
-- serialize with NixExpr.toText; evaluate with NixExpr.eval.
data NixExpr
  = NixEStr  Text
  | NixEInt  Int
  | NixEBool Bool
  | NixENull
  | NixEList [NixExpr]
  | NixEAttrs [(Text, NixExpr)]
  | NixEPath StorePath
  deriving (Eq, Ord)

instance Show NixExpr where
  show = Text.unpack . nixExpr_toText

-- | Specification for a basic fixed derivation. Construct with
-- DerivationSpec.make, then pass to Nix.mkDerivation.
data DerivationSpec = DerivationSpec
  { ds_name    :: Text
  , ds_builder :: StorePath
  , ds_system  :: Text
  , ds_args    :: [Text]
  , ds_env     :: Map Text Text
  , ds_outputs :: [Text]
  } deriving (Eq)

instance Show DerivationSpec where
  show ds = "DerivationSpec{name=" <> Text.unpack ds.ds_name <> "}"

-- | A dependency graph of a flake's inputs, parsed from flake.lock.
data FlakeGraph = FlakeGraph
  { fg_nodes :: [Text]
  , fg_edges :: [(Text, Text)]
  , fg_urls  :: Map Text Text
  } deriving (Eq)

instance Show FlakeGraph where
  show fg = "FlakeGraph{" <> show (length fg.fg_nodes) <> " nodes}"

--------------------------------------------------------------------------------
-- Process helpers: the single failure model for this module
--
-- Note: the Left message embeds `show cfg`, which for typed-process
-- includes the raw command line. All ProcessConfigs built in this
-- module carry no secrets in argv or env, so this is safe here. Do
-- not route a config produced by 'secret_setEnv' through these.

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

decodeL :: L.ByteString -> Text
decodeL = Text.decodeUtf8 . L.toStrict

-- | Run a process. Right is the stripped stdout; Left is a diagnostic
-- containing the command, exit code, and stderr.
run :: ProcessConfig stdin stdout stderr -> IO (Either Text Text)
run cfg = do
  (code, out, err) <- readProcess cfg
  pure $ case code of
    ExitSuccess -> Right (Text.strip (decodeL out))
    ExitFailure n ->
      Left $ tshow cfg <> ": exit " <> tshow n <> ": " <> Text.strip (decodeL err)

-- | Run a process expected to emit JSON on stdout.
runJson :: ProcessConfig stdin stdout stderr -> IO (Either Text Value)
runJson cfg = do
  (code, out, err) <- readProcess cfg
  pure $ case code of
    ExitFailure n ->
      Left $ tshow cfg <> ": exit " <> tshow n <> ": " <> Text.strip (decodeL err)
    ExitSuccess ->
      case Json.eitherDecode out of
        Left e  -> Left $ "invalid JSON from " <> tshow cfg <> ": " <> Text.pack e
        Right v -> Right v

--------------------------------------------------------------------------------
-- StorePath utilities

-- | The nix base-32 alphabet (no e, o, t, u).
nixBase32Chars :: String
nixBase32Chars = "0123456789abcdfghijklmnpqrsvwxyz"

-- | Validate a store path: /nix/store/<32 base32 chars>-<name>.
-- A prefix check alone is not validation; this is the invariant the
-- newtype claims to carry, so it gets checked.
storePath_fromText :: Text -> Maybe StorePath
storePath_fromText t = do
  rest <- Text.stripPrefix "/nix/store/" t
  let (hash, name) = Text.splitAt 32 rest
  guard (Text.length hash == 32)
  guard (Text.all (`elem` nixBase32Chars) hash)
  guard (Text.isPrefixOf "-" name)
  guard (Text.length name > 1)
  pure (StorePath t)

storePath_toText :: StorePath -> Text
storePath_toText (StorePath t) = t

--------------------------------------------------------------------------------
-- Secret utilities

-- | Escape hatch: yields the plaintext. Named so its use is legible
-- in scripts. Prefer 'secret_setEnv' or 'secret_writeFile'.
secret_expose :: Secret -> Text
secret_expose (Secret t) = t

-- | Add one environment variable containing a secret to a process
-- config, inheriting the current environment. The plaintext never
-- appears as a guest-language value.
secret_setEnv ::
  Text -> Secret -> ProcessConfig () () () -> IO (ProcessConfig () () ())
secret_setEnv name (Secret v) cfg = do
  env <- getEnvironment
  pure $ Process.setEnv (env ++ [(Text.unpack name, Text.unpack v)]) cfg

-- | Write a secret to a file created with mode 0600. The file is
-- opened with restrictive permissions before any content is written,
-- closing the write-then-chmod race. If the file pre-existed with
-- loose permissions, the mode is clamped, but a pre-existing open fd
-- held by another process retains access; delete-and-recreate if that
-- threat matters to you. Requires unix >= 2.8.
secret_writeFile :: Text -> Secret -> IO ()
secret_writeFile path (Secret t) = do
  let fp = Text.unpack path
  fd <-
    PosixIO.openFd
      fp
      PosixIO.WriteOnly
      PosixIO.defaultFileFlags
        { PosixIO.creat = Just 0o600
        , PosixIO.trunc = True
        }
  PosixFiles.setFileMode fp 0o600
  h <- PosixIO.fdToHandle fd
  ByteString.hPutStr h (Text.encodeUtf8 t)
  IO.hClose h

--------------------------------------------------------------------------------
-- NixHash utilities

nixHash_sha256Path :: StorePath -> IO (Either Text NixHash)
nixHash_sha256Path (StorePath p) =
  fmap (fmap NixHash) (run (proc "nix" ["hash", "path", Text.unpack p]))

nixHash_sha256Text :: Text -> IO (Either Text NixHash)
nixHash_sha256Text t =
  Temp.withSystemTempFile "nixhell-hash" $ \fp h -> do
    ByteString.hPutStr h (Text.encodeUtf8 t)
    IO.hClose h
    fmap (fmap NixHash) (run (proc "nix" ["hash", "file", fp]))

nixHash_toText :: NixHash -> Text
nixHash_toText (NixHash t) = t

--------------------------------------------------------------------------------
-- Derivation utilities

derivation_fromStorePath :: StorePath -> Maybe Derivation
derivation_fromStorePath sp@(StorePath t) =
  if Text.isSuffixOf ".drv" t
    then Just (Derivation sp)
    else Nothing

derivation_toStorePath :: Derivation -> StorePath
derivation_toStorePath (Derivation sp) = sp

--------------------------------------------------------------------------------
-- Flake utilities

flake_fromText :: Text -> Maybe Flake
flake_fromText t
  | Text.null (Text.strip t) = Nothing
  | otherwise                 = Just (Flake t)

flake_toText :: Flake -> Text
flake_toText (Flake t) = t

--------------------------------------------------------------------------------
-- NixExpr construction

nixExpr_str :: Text -> NixExpr
nixExpr_str = NixEStr

nixExpr_int :: Int -> NixExpr
nixExpr_int = NixEInt

nixExpr_bool :: Bool -> NixExpr
nixExpr_bool = NixEBool

nixExpr_true :: NixExpr
nixExpr_true = NixEBool True

nixExpr_false :: NixExpr
nixExpr_false = NixEBool False

nixExpr_null :: NixExpr
nixExpr_null = NixENull

nixExpr_list :: [NixExpr] -> NixExpr
nixExpr_list = NixEList

nixExpr_attrs :: [(Text, NixExpr)] -> NixExpr
nixExpr_attrs = NixEAttrs

nixExpr_path :: StorePath -> NixExpr
nixExpr_path = NixEPath

-- | Serialize a NixExpr to valid Nix language text.
nixExpr_toText :: NixExpr -> Text
nixExpr_toText = go
  where
    go = \case
      NixEStr t              -> "\"" <> escStr t <> "\""
      NixEInt i              -> Text.pack (show i)
      NixEBool True          -> "true"
      NixEBool False         -> "false"
      NixENull               -> "null"
      NixEPath (StorePath p) -> p
      NixEList xs            -> "[ " <> Text.unwords (map go xs) <> " ]"
      NixEAttrs kvs          -> "{ " <> Text.concat (map renderAttr kvs) <> "}"
    renderAttr (k, v) = k <> " = " <> go v <> "; "
    escStr t =
      Text.replace "${" "\\${"
      $ Text.replace "\"" "\\\""
      $ Text.replace "\\" "\\\\" t

-- | Evaluate a NixExpr, returning the result as a JSON Value.
nixExpr_eval :: NixExpr -> IO (Either Text Value)
nixExpr_eval expr =
  runJson (proc "nix" ["eval", "--json", "--expr", Text.unpack (nixExpr_toText expr)])

--------------------------------------------------------------------------------
-- DerivationSpec

derivationSpec_make ::
  Text -> StorePath -> Text -> [Text] -> Map Text Text -> [Text] ->
  DerivationSpec
derivationSpec_make name builder system args env outputs =
  DerivationSpec
    { ds_name    = name
    , ds_builder = builder
    , ds_system  = system
    , ds_args    = args
    , ds_env     = env
    , ds_outputs = outputs
    }

-- | Instantiate a DerivationSpec to a .drv file without building it.
nix_mkDerivation :: DerivationSpec -> IO (Either Text Derivation)
nix_mkDerivation ds = do
  r <- run (proc "nix-instantiate" ["--expr", Text.unpack (derivationSpecToNix ds)])
  pure $ do
    out <- r
    case filter (not . Text.null) (Text.lines out) of
      (t : _) ->
        case derivation_fromStorePath (StorePath t) of
          Just drv -> Right drv
          Nothing  -> Left ("Nix.mkDerivation: result is not a .drv path: " <> t)
      [] -> Left "Nix.mkDerivation: nix-instantiate produced no output"

-- | Build a derivation, returning its output store paths keyed by output name.
nix_realise :: Derivation -> IO (Either Text (Map Text StorePath))
nix_realise drv = do
  r <- run
    (proc "nix-store"
      ["--realise", Text.unpack (storePath_toText (derivation_toStorePath drv))])
  pure $ fmap toMap r
  where
    toMap out =
      case filter (not . Text.null) (Text.lines out) of
        [p] -> Map.singleton "out" (StorePath p)
        ps  -> Map.fromList
                 (zip (map (\i -> "out" <> Text.pack (show (i :: Int))) [0 ..])
                      (map StorePath ps))

-- | Internal: convert a DerivationSpec to a Nix derivation expression.
--
-- The builder StorePath is split into base (/nix/store/hash-name) and
-- subpath (/bin/bash). builtins.storePath on the base registers it as a
-- proper derivation input so the build sandbox can access it. A bare
-- string would not be added to inputSrcs and the binary would be missing
-- inside the sandbox despite the path being syntactically correct.
derivationSpecToNix :: DerivationSpec -> Text
derivationSpecToNix ds =
  "let __builder = builtins.storePath "
  <> nixStr storeBase
  <> "; in derivation { "
  <> Text.unwords fields
  <> " }"
  where
    builderText = storePath_toText ds.ds_builder
    storeBase   = Text.intercalate "/" (take 4 (Text.splitOn "/" builderText))
    subPath     = Text.drop (Text.length storeBase) builderText
    builderExpr =
      if Text.null subPath
        then "__builder"
        else "__builder + " <> nixStr subPath
    fields =
      [ "name = "    <> nixStr ds.ds_name <> ";"
      , "builder = " <> builderExpr <> ";"
      , "system = "  <> nixStr ds.ds_system <> ";"
      ]
      ++ (if null ds.ds_args then []
          else ["args = [ " <> Text.unwords (map nixStr ds.ds_args) <> " ];"])
      ++ (if null ds.ds_outputs then []
          else ["outputs = [ " <> Text.unwords (map nixStr ds.ds_outputs) <> " ];"])
      ++ [ k <> " = " <> nixStr v <> ";"
         | (k, v) <- Map.toList ds.ds_env ]
    nixStr t =
      "\"" <> Text.replace "\\" "\\\\" (Text.replace "\"" "\\\"" t) <> "\""

--------------------------------------------------------------------------------
-- FlakeGraph analysis

nix_flakeGraph :: Text -> IO (Either Text FlakeGraph)
nix_flakeGraph dir = do
  r <- nix_flakeLock dir
  pure $ fmap graphOf r
  where
    graphOf v = case v of
      Json.Object top ->
        case Map.lookup "nodes" (KeyMap.toMapText top) of
          Just (Json.Object nodes) ->
            let nodeMap  = KeyMap.toMapText nodes
                allNodes = filter (/= "root") $ Map.keys nodeMap
                edges =
                  [ (parent, child)
                  | (parent, Json.Object node) <- Map.toList nodeMap
                  , parent /= "root"
                  , Just (Json.Object inputs) <-
                      [Map.lookup "inputs" (KeyMap.toMapText node)]
                  , (_, Json.String child) <-
                      Map.toList (KeyMap.toMapText inputs)
                  ]
                urls = Map.fromList
                  [ (k, describeLockedNode (KeyMap.toMapText locked))
                  | (k, Json.Object node) <- Map.toList nodeMap
                  , k /= "root"
                  , Just (Json.Object locked) <-
                      [Map.lookup "locked" (KeyMap.toMapText node)]
                  ]
            in FlakeGraph { fg_nodes = allNodes, fg_edges = edges, fg_urls = urls }
          _ -> FlakeGraph { fg_nodes = [], fg_edges = [], fg_urls = Map.empty }
      _ -> FlakeGraph { fg_nodes = [], fg_edges = [], fg_urls = Map.empty }

flakeGraph_nodes :: FlakeGraph -> [Text]
flakeGraph_nodes = fg_nodes

flakeGraph_edges :: FlakeGraph -> [(Text, Text)]
flakeGraph_edges = fg_edges

flakeGraph_urls :: FlakeGraph -> Map Text Text
flakeGraph_urls = fg_urls

flakeGraph_detectCycles :: FlakeGraph -> Maybe [Text]
flakeGraph_detectCycles fg =
  foldl
    (\acc node -> case acc of
      Just c  -> Just c
      Nothing -> dfs [] node)
    Nothing
    (fg_nodes fg)
  where
    adjacency :: Map Text [Text]
    adjacency = Map.fromListWith (++)
      [ (from, [to]) | (from, to) <- fg_edges fg ]

    neighbours :: Text -> [Text]
    neighbours node = case Map.lookup node adjacency of
      Nothing -> []
      Just ns -> ns

    dfs :: [Text] -> Text -> Maybe [Text]
    dfs stack node
      | node `elem` stack =
          Just (dropWhile (/= node) stack ++ [node])
      | otherwise =
          foldl
            (\acc child -> case acc of
              Just c  -> Just c
              Nothing -> dfs (stack ++ [node]) child)
            Nothing
            (neighbours node)

--------------------------------------------------------------------------------
-- Cache: persistent KV store in ~/.cache/nix-hell/
--
-- Keys are hex-escaped so distinct keys never share a file (the old
-- scheme mapped both '/' and ':' to '_', a collision, and permitted
-- ".." as a filename). Writes are temp-file-plus-rename so concurrent
-- readers never observe torn data.

cacheDir :: IO FilePath
cacheDir = do
  home <- getEnv "HOME"
  let dir = home <> "/.cache/nix-hell"
  Dir.createDirectoryIfMissing True dir
  pure dir

sanitizeKey :: Text -> String
sanitizeKey = Text.unpack . Text.concatMap enc
  where
    enc c
      | isAscii c && isAlphaNum c = Text.singleton c
      | c == '-'                  = Text.singleton c
      | otherwise = "_" <> Text.pack (showHex (ord c) "") <> "_"

cacheFile :: Text -> IO FilePath
cacheFile key = do
  dir <- cacheDir
  pure (dir <> "/k-" <> sanitizeKey key)

cache_get :: Text -> IO (Maybe Text)
cache_get key = do
  fp <- cacheFile key
  exists <- Dir.doesFileExist fp
  if exists
    then fmap (Just . Text.decodeUtf8) (ByteString.readFile fp)
    else pure Nothing

cache_set :: Text -> Text -> IO ()
cache_set key val = do
  dir <- cacheDir
  fp <- cacheFile key
  (tmp, h) <- IO.openTempFile dir "write"
  ByteString.hPutStr h (Text.encodeUtf8 val)
  IO.hClose h
  Dir.renameFile tmp fp

cache_getOrRun :: Text -> IO Text -> IO Text
cache_getOrRun key action = do
  mval <- cache_get key
  case mval of
    Just v  -> pure v
    Nothing -> do
      v <- action
      cache_set key v
      pure v

cache_invalidate :: Text -> IO ()
cache_invalidate key = do
  fp <- cacheFile key
  exists <- Dir.doesFileExist fp
  if exists then Dir.removeFile fp else pure ()

--------------------------------------------------------------------------------
-- Nix store operations

nix_build :: Text -> IO (Either Text StorePath)
nix_build attr = do
  r <- run (proc "nix" ["build", "--no-link", "--print-out-paths", Text.unpack attr])
  pure $ do
    out <- r
    case filter (not . Text.null) (Text.lines out) of
      (p : _) -> Right (StorePath p)
      []      -> Left ("Nix.build: no output paths for " <> attr)

nix_buildFlakeAttr :: Text -> Text -> IO (Either Text StorePath)
nix_buildFlakeAttr flake attr = nix_build (flake <> "#" <> attr)

nix_eval :: Text -> IO (Either Text Value)
nix_eval expr =
  runJson (proc "nix" ["eval", "--json", "--expr", Text.unpack expr])

nix_evalFlakeAttr :: Text -> Text -> IO (Either Text Value)
nix_evalFlakeAttr flake attr =
  runJson (proc "nix" ["eval", "--json", Text.unpack (flake <> "#" <> attr)])

nix_instantiate :: Text -> IO (Either Text Derivation)
nix_instantiate expr = do
  r <- run (proc "nix-instantiate" ["--expr", Text.unpack expr])
  pure $ do
    out <- r
    case filter (not . Text.null) (Text.lines out) of
      (t : _) ->
        case derivation_fromStorePath (StorePath t) of
          Just drv -> Right drv
          Nothing  -> Left ("Nix.instantiate: result is not a .drv path: " <> t)
      [] -> Left "Nix.instantiate: no output"

nix_storeAdd :: Text -> ByteString -> IO (Either Text StorePath)
nix_storeAdd name contents =
  Temp.withSystemTempFile (Text.unpack name) $ \fp h -> do
    ByteString.hPutStr h contents
    IO.hClose h
    r <- run (proc "nix" ["store", "add-file", "--name", Text.unpack name, fp])
    pure (fmap StorePath r)

nix_isInStore :: Text -> IO Bool
nix_isInStore path = do
  code <- runProcess
    (setStdout nullStream (setStderr nullStream
      (proc "nix" ["store", "ls", Text.unpack path])))
  pure $ case code of
    ExitSuccess -> True
    _           -> False

nix_queryRequisites :: StorePath -> IO (Either Text [StorePath])
nix_queryRequisites (StorePath p) = do
  r <- run (proc "nix-store" ["--query", "--requisites", Text.unpack p])
  pure $ fmap (map StorePath . filter (not . Text.null) . Text.lines) r

nix_copy :: StorePath -> Text -> IO (Either Text Text)
nix_copy (StorePath p) dest =
  run (proc "nix" ["copy", "--to", Text.unpack dest, Text.unpack p])

nix_sign :: StorePath -> Text -> IO (Either Text Text)
nix_sign (StorePath p) keyFile =
  run (proc "nix" ["store", "sign", "--key-file", Text.unpack keyFile, Text.unpack p])

--------------------------------------------------------------------------------
-- Nix flake operations

nix_flakeMetadata :: Text -> IO (Either Text Value)
nix_flakeMetadata flake =
  runJson (proc "nix" ["flake", "metadata", "--json", Text.unpack flake])

nix_flakeUpdate :: Text -> IO (Either Text Text)
nix_flakeUpdate dir =
  run (setWorkingDir (Text.unpack dir) (proc "nix" ["flake", "update"]))

nix_flakeLock :: Text -> IO (Either Text Value)
nix_flakeLock dir = do
  r <- try (L.readFile (Text.unpack dir <> "/flake.lock"))
  pure $ case r of
    Left (e :: IOException) ->
      Left ("Nix.flakeLock: " <> tshow e)
    Right contents ->
      case Json.eitherDecode contents of
        Left e  -> Left ("Nix.flakeLock: could not parse flake.lock: " <> Text.pack e)
        Right v -> Right v

nix_flakeInputs :: Text -> IO (Either Text (Map Text Text))
nix_flakeInputs dir = do
  r <- nix_flakeLock dir
  pure $ fmap inputsOf r
  where
    inputsOf v = case v of
      Json.Object top ->
        case Map.lookup "nodes" (KeyMap.toMapText top) of
          Just (Json.Object nodes) ->
            Map.fromList
              [ (k, describeLockedNode (KeyMap.toMapText locked))
              | (k, Json.Object node) <- Map.toList (KeyMap.toMapText nodes)
              , k /= "root"
              , Just (Json.Object locked) <-
                  [Map.lookup "locked" (KeyMap.toMapText node)]
              ]
          _ -> Map.empty
      _ -> Map.empty

nix_checkFlakeOutputs :: Text -> IO (Either Text Text)
nix_checkFlakeOutputs dir =
  run (proc "nix" ["flake", "check", Text.unpack dir])

describeLockedNode :: Map Text Json.Value -> Text
describeLockedNode m =
  case Map.lookup "type" m of
    Just (Json.String "github") ->
      "github:" <> strField "owner" <> "/" <> strField "repo"
                <> "/" <> Text.take 7 (strField "rev")
    Just (Json.String "git") ->
      strField "url" <> "@" <> Text.take 7 (strField "rev")
    Just (Json.String "path") ->
      strField "path"
    Just (Json.String t) ->
      t <> ":" <> strField "url"
    _ -> "<unknown>"
  where
    strField k = case Map.lookup k m of
      Just (Json.String v) -> v
      _                    -> ""

--------------------------------------------------------------------------------
-- Profile and GC

nix_profileInstall :: StorePath -> IO (Either Text Text)
nix_profileInstall (StorePath p) =
  run (proc "nix" ["profile", "install", Text.unpack p])

nix_profileRemove :: Text -> IO (Either Text Text)
nix_profileRemove name =
  run (proc "nix" ["profile", "remove", Text.unpack name])

nix_profileList :: IO (Either Text [StorePath])
nix_profileList = do
  r <- runJson (proc "nix" ["profile", "list", "--json"])
  pure $ fmap pathsOf r
  where
    pathsOf v = case v of
      Json.Object top ->
        case Map.lookup "elements" (KeyMap.toMapText top) of
          Just (Json.Array elems) ->
            concatMap extractStorePaths (foldr (:) [] elems)
          _ -> []
      _ -> []
    extractStorePaths :: Json.Value -> [StorePath]
    extractStorePaths (Json.Object m) =
      case Map.lookup "storePaths" (KeyMap.toMapText m) of
        Just (Json.Array paths) ->
          [ StorePath t | Json.String t <- foldr (:) [] paths ]
        _ -> []
    extractStorePaths _ = []

nix_gcCollect :: IO (Either Text Text)
nix_gcCollect = run (proc "nix-collect-garbage" [])

nix_gcRoots :: IO (Either Text [StorePath])
nix_gcRoots = do
  r <- run (proc "nix-store" ["--gc", "--print-roots"])
  pure $ fmap rootsOf r
  where
    rootsOf =
      map (StorePath . Text.strip . takeLeft . Text.splitOn " -> ")
      . filter (not . Text.null)
      . Text.lines
    takeLeft (x : _) = x
    takeLeft []      = ""

nix_addRoot :: StorePath -> Text -> IO (Either Text Text)
nix_addRoot (StorePath p) rootPath =
  run (proc "nix-store"
    ["--add-root", Text.unpack rootPath, "--indirect",
     "--realise", Text.unpack p])

nix_optimiseStore :: IO (Either Text Text)
nix_optimiseStore = run (proc "nix" ["store", "optimise"])

--------------------------------------------------------------------------------
-- Sops operations

sops_get :: Text -> Text -> IO (Either Text Secret)
sops_get file key = do
  r <- runJson (proc "sops" ["--decrypt", "--output-type", "json", Text.unpack file])
  pure $ do
    v <- r
    case v of
      Json.Object m ->
        case Map.lookup key (KeyMap.toMapText m) of
          Just (Json.String s) -> Right (Secret s)
          Just _  -> Left ("Sops.get: key is not a string: " <> key)
          Nothing -> Left ("Sops.get: key not found: " <> key)
      _ -> Left "Sops.get: sops output was not a JSON object"

sops_getAll :: Text -> IO (Either Text (Map Text Secret))
sops_getAll file = do
  r <- runJson (proc "sops" ["--decrypt", "--output-type", "json", Text.unpack file])
  pure $ do
    v <- r
    case v of
      Json.Object m ->
        Right $ Map.fromList
          [ (k, Secret s)
          | (k, Json.String s) <- Map.toList (KeyMap.toMapText m)
          ]
      _ -> Left "Sops.getAll: sops output was not a JSON object"

--------------------------------------------------------------------------------
-- Age operations
--
-- Armored (-a) so ciphertext is Text-safe and survives copy-paste,
-- version control, and sops-style embedding.

age_encrypt :: Text -> Text -> IO (Either Text Text)
age_encrypt pubkey plaintext = do
  (code, out, err) <- readProcess
    (setStdin
      (byteStringInput (L.fromStrict (Text.encodeUtf8 plaintext)))
      (proc "age" ["--encrypt", "--armor", "--recipient", Text.unpack pubkey]))
  pure $ case code of
    ExitSuccess   -> Right (decodeL out)
    ExitFailure n ->
      Left ("Age.encrypt: exit " <> tshow n <> ": " <> Text.strip (decodeL err))

age_decrypt :: Text -> Text -> IO (Either Text Secret)
age_decrypt identityFile ciphertext = do
  (code, out, err) <- readProcess
    (setStdin
      (byteStringInput (L.fromStrict (Text.encodeUtf8 ciphertext)))
      (proc "age" ["--decrypt", "--identity", Text.unpack identityFile]))
  pure $ case code of
    ExitSuccess   -> Right (Secret (decodeL out))
    ExitFailure n ->
      Left ("Age.decrypt: exit " <> tshow n <> ": " <> Text.strip (decodeL err))

ssh_toAge :: Text -> IO (Either Text Text)
ssh_toAge pubkeyPath =
  run (proc "ssh-to-age" ["-i", Text.unpack pubkeyPath])

--------------------------------------------------------------------------------
-- Shell safety
--
-- Note: Process.proc never invokes a shell, so escaping is NOT needed
-- for local process invocation. These exist for the one legitimate
-- case: constructing a command string to hand to a remote shell, e.g.
-- ssh host "nix-store --realise <escaped>".

shell_escape :: Text -> Text
shell_escape t = "'" <> Text.replace "'" "'\\''" t <> "'"

shell_escapeList :: [Text] -> Text
shell_escapeList ts = Text.intercalate " " (map shell_escape ts)

-- | The old version returned StorePath, which was a lie: `which` output
-- is only a store path on a pure-NixOS PATH. Returns the raw path;
-- validate with StorePath.fromText if you need the invariant.
shell_which :: Text -> IO (Maybe Text)
shell_which cmd = do
  r <- run (proc "which" [Text.unpack cmd])
  pure $ either (const Nothing) Just r

shell_inPath :: Text -> IO Bool
shell_inPath cmd = do
  code <- runProcess
    (setStdout nullStream (setStderr nullStream (proc "which" [Text.unpack cmd])))
  pure $ case code of
    ExitSuccess -> True
    _           -> False

--------------------------------------------------------------------------------
-- NixOS operations

-- | Streams output to the terminal; returns the exit code. Kept as
-- ExitCode rather than Either because rebuild output is interactive
-- and you want to see it live.
nixos_rebuild :: Text -> IO ExitCode
nixos_rebuild action =
  runProcess (proc "nixos-rebuild" [Text.unpack action])

nixos_currentSystem :: IO (Either Text StorePath)
nixos_currentSystem = do
  r <- run (proc "readlink" ["-f", "/run/current-system"])
  pure (fmap StorePath r)

-- | Evaluate a NixOS option from a flake-based system configuration.
-- Takes the flake dir explicitly (the old version hardcoded /etc/nixos)
-- and evaluates purely via the attrpath (the old version used --impure
-- with builtins.getFlake). Impure evaluation failures become Left.
nixos_option :: Text -> Text -> IO (Either Text Value)
nixos_option flakeDir option = do
  hr <- run (proc "hostname" [])
  case hr of
    Left e -> pure (Left e)
    Right hostname ->
      runJson
        (proc "nix"
          [ "eval", "--json"
          , Text.unpack
              (flakeDir <> "#nixosConfigurations." <> hostname
                        <> ".config." <> option)
          ])

nixos_generations :: IO (Either Text Text)
nixos_generations = do
  r <- try (Dir.listDirectory "/nix/var/nix/profiles")
  pure $ case r of
    Left (e :: IOException) -> Left ("NixOS.generations: " <> tshow e)
    Right entries ->
      Right $ Text.unlines
        [ t | e <- entries, let t = Text.pack e, Text.isPrefixOf "system-" t ]

nixos_rollback :: IO (Either Text Text)
nixos_rollback = run (proc "nixos-rebuild" ["--rollback", "switch"])

--------------------------------------------------------------------------------
-- Systemd operations
--
-- status/logs capture output regardless of exit code (systemctl status
-- exits 3 for inactive units, which is information, not failure).

systemd_status :: Text -> IO Text
systemd_status unit = do
  (_code, out, _err) <-
    readProcess (proc "systemctl" ["status", Text.unpack unit])
  pure (decodeL out)

systemd_start :: Text -> IO ExitCode
systemd_start unit =
  runProcess (proc "systemctl" ["start", Text.unpack unit])

systemd_stop :: Text -> IO ExitCode
systemd_stop unit =
  runProcess (proc "systemctl" ["stop", Text.unpack unit])

systemd_restart :: Text -> IO ExitCode
systemd_restart unit =
  runProcess (proc "systemctl" ["restart", Text.unpack unit])

systemd_logs :: Text -> IO Text
systemd_logs unit = do
  (_code, out, _err) <-
    readProcess (proc "journalctl" ["-u", Text.unpack unit, "--no-pager"])
  pure (decodeL out)

--------------------------------------------------------------------------------
-- Registration lists: the entire integration surface with Hell.hs
--
-- Hell.hs splices these into its three tables. Adding a primitive or
-- type means editing this section only; upstream rebases never touch
-- NixHell code paths in Hell.hs beyond three one-line splices.

-- | Guest-visible type constructors.
nixTypes :: [(String, SomeTypeRep)]
nixTypes =
  [ ("StorePath",      SomeTypeRep (typeRep @StorePath))
  , ("Secret",         SomeTypeRep (typeRep @Secret))
  , ("NixHash",        SomeTypeRep (typeRep @NixHash))
  , ("Derivation",     SomeTypeRep (typeRep @Derivation))
  , ("Flake",          SomeTypeRep (typeRep @Flake))
  , ("NixExpr",        SomeTypeRep (typeRep @NixExpr))
  , ("DerivationSpec", SomeTypeRep (typeRep @DerivationSpec))
  , ("FlakeGraph",     SomeTypeRep (typeRep @FlakeGraph))
  ]

-- | Monomorphic instance dictionaries for guest-visible constraints.
-- Only plain instances live here; containers of NixHell types resolve
-- through Hell's entailment machinery recursing into these.
nixInstances :: [((SomeTypeRep, SomeTypeRep), Dynamic)]
nixInstances =
  [ inst0 @Show @StorePath
  , inst0 @Eq   @StorePath
  , inst0 @Ord  @StorePath
  , inst0 @Show @NixHash
  , inst0 @Eq   @NixHash
  , inst0 @Ord  @NixHash
  , inst0 @Show @Derivation
  , inst0 @Eq   @Derivation
  , inst0 @Ord  @Derivation
  , inst0 @Show @Flake
  , inst0 @Eq   @Flake
  , inst0 @Ord  @Flake
  , inst0 @Show @NixExpr
  , inst0 @Eq   @NixExpr
  , inst0 @Ord  @NixExpr
  , inst0 @Show @DerivationSpec
  , inst0 @Eq   @DerivationSpec
  , inst0 @Show @FlakeGraph
  , inst0 @Eq   @FlakeGraph
  ]
  where
    inst0 ::
      forall (cls :: Type -> Constraint) a.
      (cls a, Typeable cls, Typeable a) =>
      ((SomeTypeRep, SomeTypeRep), Dynamic)
    inst0 =
      ( (SomeTypeRep (typeRep @cls), SomeTypeRep (typeRep @a))
      , toDyn (Dict @(cls a))
      )

-- | Guest-visible primitives as (name, Dynamic). Hell.hs unwraps the
-- Dynamic into (TypeRep a, a) and registers it as a literal.
nixLits :: [(String, Dynamic)]
nixLits =
  [ -- StorePath
    l "StorePath.fromText"       storePath_fromText
  , l "StorePath.toText"         storePath_toText
    -- Secret
  , l "Secret.expose"            secret_expose
  , l "Secret.setEnv"            secret_setEnv
  , l "Secret.writeFile"         secret_writeFile
    -- NixHash
  , l "NixHash.sha256Path"       nixHash_sha256Path
  , l "NixHash.sha256Text"       nixHash_sha256Text
  , l "NixHash.toText"           nixHash_toText
    -- Derivation
  , l "Derivation.fromStorePath" derivation_fromStorePath
  , l "Derivation.toStorePath"   derivation_toStorePath
    -- Flake
  , l "Flake.fromText"           flake_fromText
  , l "Flake.toText"             flake_toText
    -- NixExpr
  , l "NixExpr.str"              nixExpr_str
  , l "NixExpr.int"              nixExpr_int
  , l "NixExpr.bool"             nixExpr_bool
  , l "NixExpr.true"             nixExpr_true
  , l "NixExpr.false"            nixExpr_false
  , l "NixExpr.null"             nixExpr_null
  , l "NixExpr.list"             nixExpr_list
  , l "NixExpr.attrs"            nixExpr_attrs
  , l "NixExpr.path"             nixExpr_path
  , l "NixExpr.toText"           nixExpr_toText
  , l "NixExpr.eval"             nixExpr_eval
    -- DerivationSpec
  , l "DerivationSpec.make"      derivationSpec_make
  , l "Nix.mkDerivation"         nix_mkDerivation
  , l "Nix.realise"              nix_realise
    -- FlakeGraph
  , l "Nix.flakeGraph"           nix_flakeGraph
  , l "FlakeGraph.nodes"         flakeGraph_nodes
  , l "FlakeGraph.edges"         flakeGraph_edges
  , l "FlakeGraph.urls"          flakeGraph_urls
  , l "FlakeGraph.detectCycles"  flakeGraph_detectCycles
    -- Cache
  , l "Cache.get"                cache_get
  , l "Cache.set"                cache_set
  , l "Cache.getOrRun"           cache_getOrRun
  , l "Cache.invalidate"         cache_invalidate
    -- Nix store
  , l "Nix.build"                nix_build
  , l "Nix.buildFlakeAttr"       nix_buildFlakeAttr
  , l "Nix.storeAdd"             nix_storeAdd
  , l "Nix.isInStore"            nix_isInStore
  , l "Nix.queryRequisites"      nix_queryRequisites
  , l "Nix.copy"                 nix_copy
  , l "Nix.sign"                 nix_sign
    -- Nix eval and flake
  , l "Nix.eval"                 nix_eval
  , l "Nix.evalFlakeAttr"        nix_evalFlakeAttr
  , l "Nix.instantiate"          nix_instantiate
  , l "Nix.flakeMetadata"        nix_flakeMetadata
  , l "Nix.flakeUpdate"          nix_flakeUpdate
  , l "Nix.flakeLock"            nix_flakeLock
  , l "Nix.flakeInputs"          nix_flakeInputs
  , l "Nix.checkFlakeOutputs"    nix_checkFlakeOutputs
    -- Profile and GC
  , l "Nix.profileInstall"       nix_profileInstall
  , l "Nix.profileRemove"        nix_profileRemove
  , l "Nix.profileList"          nix_profileList
  , l "Nix.gcCollect"            nix_gcCollect
  , l "Nix.gcRoots"              nix_gcRoots
  , l "Nix.addRoot"              nix_addRoot
  , l "Nix.optimiseStore"        nix_optimiseStore
    -- Sops
  , l "Sops.get"                 sops_get
  , l "Sops.getAll"              sops_getAll
    -- Age
  , l "Age.encrypt"              age_encrypt
  , l "Age.decrypt"              age_decrypt
  , l "Ssh.toAge"                ssh_toAge
    -- Shell
  , l "Shell.escape"             shell_escape
  , l "Shell.escapeList"         shell_escapeList
  , l "Shell.which"              shell_which
  , l "Shell.inPath"             shell_inPath
    -- NixOS
  , l "NixOS.rebuild"            nixos_rebuild
  , l "NixOS.currentSystem"      nixos_currentSystem
  , l "NixOS.option"             nixos_option
  , l "NixOS.generations"        nixos_generations
  , l "NixOS.rollback"           nixos_rollback
    -- Systemd
  , l "Systemd.status"           systemd_status
  , l "Systemd.start"            systemd_start
  , l "Systemd.stop"             systemd_stop
  , l "Systemd.restart"          systemd_restart
  , l "Systemd.logs"             systemd_logs
  ]
  where
    l :: forall a. (Typeable a) => String -> a -> (String, Dynamic)
    l name x = (name, toDyn x)