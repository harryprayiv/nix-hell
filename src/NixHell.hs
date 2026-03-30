{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
    -- StorePath
  , storePath_fromText
  , storePath_toText
    -- Secret
  , secret_toEnvValue
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

import Data.Aeson (Value)
import qualified Data.Aeson as Json
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified System.Directory as Dir
import System.Environment (getEnv)
import qualified System.IO as IO
import System.Process.Typed as Process
import qualified System.IO.Temp as Temp

--------------------------------------------------------------------------------
-- Types

-- | A validated Nix store path. Opaque: only constructible by
-- trusted primitives. Scripts cannot produce one from raw Text.
newtype StorePath = StorePath Text
  deriving (Eq, Ord)

instance Show StorePath where
  show (StorePath t) = Text.unpack t

-- | An opaque secret value. No Show instance: secrets cannot flow
-- into display, logging, or string concatenation.
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
  deriving (Eq)

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
  deriving (Eq)

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
-- StorePath utilities

storePath_fromText :: Text -> Maybe StorePath
storePath_fromText t =
  if Text.isPrefixOf "/nix/store/" t
    then Just (StorePath t)
    else Nothing

storePath_toText :: StorePath -> Text
storePath_toText (StorePath t) = t

--------------------------------------------------------------------------------
-- Secret utilities

secret_toEnvValue :: Secret -> Text
secret_toEnvValue (Secret t) = t

secret_writeFile :: Text -> Secret -> IO ()
secret_writeFile path (Secret t) = do
  ByteString.writeFile (Text.unpack path) (Text.encodeUtf8 t)
  runProcess_ (proc "chmod" ["600", Text.unpack path])

--------------------------------------------------------------------------------
-- NixHash utilities

nixHash_sha256Path :: StorePath -> IO NixHash
nixHash_sha256Path (StorePath p) = do
  out <- readProcessStdout_
    (proc "nix" ["hash", "path", Text.unpack p])
  pure $ NixHash $ Text.strip $ Text.decodeUtf8 $ L.toStrict out

nixHash_sha256Text :: Text -> IO NixHash
nixHash_sha256Text t =
  Temp.withSystemTempFile "nixhell-hash" $ \fp h -> do
    ByteString.hPutStr h (Text.encodeUtf8 t)
    IO.hClose h
    out <- readProcessStdout_ (proc "nix" ["hash", "file", fp])
    pure $ NixHash $ Text.strip $ Text.decodeUtf8 $ L.toStrict out

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

-- | Evaluate a NixExpr and return the result as a JSON Value.
nixExpr_eval :: NixExpr -> IO Value
nixExpr_eval expr = do
  out <- readProcessStdout_
    (proc "nix" ["eval", "--json", "--expr", Text.unpack (nixExpr_toText expr)])
  case Json.decode out of
    Nothing -> error "NixExpr.eval: nix returned invalid JSON"
    Just v  -> pure v

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
nix_mkDerivation :: DerivationSpec -> IO Derivation
nix_mkDerivation ds = do
  let expr = derivationSpecToNix ds
  out <- readProcessStdout_
    (proc "nix-instantiate" ["--expr", Text.unpack expr])
  let t = Text.strip $ Text.decodeUtf8 $ L.toStrict out
  case derivation_fromStorePath (StorePath t) of
    Just drv -> pure drv
    Nothing  ->
      error $ "Nix.mkDerivation: result is not a .drv path: " <> Text.unpack t

-- | Build a derivation, returning its output store paths keyed by output name.
nix_realise :: Derivation -> IO (Map Text StorePath)
nix_realise drv = do
  out <- readProcessStdout_
    (proc "nix-store"
      ["--realise", Text.unpack (storePath_toText (derivation_toStorePath drv))])
  let paths = filter (not . Text.null)
            $ Text.lines
            $ Text.decodeUtf8
            $ L.toStrict out
  case paths of
    [p] -> pure $ Map.singleton "out" (StorePath p)
    ps  -> pure $ Map.fromList
             $ zip (map (\i -> "out" <> Text.pack (show (i :: Int))) [0..])
                   (map StorePath ps)

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

nix_flakeGraph :: Text -> IO FlakeGraph
nix_flakeGraph dir = do
  v <- nix_flakeLock dir
  pure $ case v of
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

cacheDir :: IO FilePath
cacheDir = do
  home <- getEnv "HOME"
  let dir = home <> "/.cache/nix-hell"
  Dir.createDirectoryIfMissing True dir
  pure dir

sanitizeKey :: Text -> String
sanitizeKey = Text.unpack . Text.map safeChar
  where
    safeChar '/' = '_'
    safeChar ':' = '_'
    safeChar c   = c

cache_get :: Text -> IO (Maybe Text)
cache_get key = do
  dir <- cacheDir
  let fp = dir <> "/" <> sanitizeKey key
  exists <- Dir.doesFileExist fp
  if exists
    then fmap (Just . Text.decodeUtf8) (ByteString.readFile fp)
    else pure Nothing

cache_set :: Text -> Text -> IO ()
cache_set key val = do
  dir <- cacheDir
  let fp = dir <> "/" <> sanitizeKey key
  ByteString.writeFile fp (Text.encodeUtf8 val)

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
  dir <- cacheDir
  let fp = dir <> "/" <> sanitizeKey key
  exists <- Dir.doesFileExist fp
  if exists then Dir.removeFile fp else pure ()

--------------------------------------------------------------------------------
-- Nix store operations

nix_build :: Text -> IO StorePath
nix_build attr = do
  out <- readProcessStdout_
    (proc "nix" ["build", "--no-link", "--print-out-paths", Text.unpack attr])
  let paths = filter (not . Text.null)
            $ Text.lines
            $ Text.decodeUtf8
            $ L.toStrict out
  case paths of
    (p:_) -> pure $ StorePath p
    []    -> error $ "Nix.build: no output paths for " <> Text.unpack attr

nix_buildFlakeAttr :: Text -> Text -> IO StorePath
nix_buildFlakeAttr flake attr =
  nix_build (flake <> "#" <> attr)

nix_eval :: Text -> IO Value
nix_eval expr = do
  out <- readProcessStdout_
    (proc "nix" ["eval", "--json", "--expr", Text.unpack expr])
  case Json.decode out of
    Nothing -> error "Nix.eval: nix returned invalid JSON"
    Just v  -> pure v

nix_evalFlakeAttr :: Text -> Text -> IO Value
nix_evalFlakeAttr flake attr = do
  out <- readProcessStdout_
    (proc "nix" ["eval", "--json", Text.unpack (flake <> "#" <> attr)])
  case Json.decode out of
    Nothing -> error "Nix.evalFlakeAttr: nix returned invalid JSON"
    Just v  -> pure v

nix_instantiate :: Text -> IO Derivation
nix_instantiate expr = do
  out <- readProcessStdout_
    (proc "nix-instantiate" ["--expr", Text.unpack expr])
  let t = Text.strip $ Text.decodeUtf8 $ L.toStrict out
  case derivation_fromStorePath (StorePath t) of
    Just drv -> pure drv
    Nothing  ->
      error $ "Nix.instantiate: result is not a .drv path: " <> Text.unpack t

nix_storeAdd :: Text -> ByteString -> IO StorePath
nix_storeAdd name contents =
  Temp.withSystemTempFile (Text.unpack name) $ \fp h -> do
    ByteString.hPutStr h contents
    IO.hClose h
    out <- readProcessStdout_
      (proc "nix" ["store", "add-file", "--name", Text.unpack name, fp])
    pure $ StorePath $ Text.strip $ Text.decodeUtf8 $ L.toStrict out

nix_isInStore :: Text -> IO Bool
nix_isInStore path = do
  code <- runProcess (proc "nix" ["store", "ls", Text.unpack path])
  pure $ case code of
    ExitSuccess -> True
    _           -> False

nix_queryRequisites :: StorePath -> IO [StorePath]
nix_queryRequisites (StorePath p) = do
  out <- readProcessStdout_
    (proc "nix-store" ["--query", "--requisites", Text.unpack p])
  pure
    $ map StorePath
    $ filter (not . Text.null)
    $ Text.lines
    $ Text.decodeUtf8
    $ L.toStrict out

nix_copy :: StorePath -> Text -> IO ()
nix_copy (StorePath p) dest =
  runProcess_
    (proc "nix" ["copy", "--to", Text.unpack dest, Text.unpack p])

nix_sign :: StorePath -> Text -> IO ()
nix_sign (StorePath p) keyFile =
  runProcess_
    (proc "nix" ["store", "sign",
                 "--key-file", Text.unpack keyFile,
                 Text.unpack p])

--------------------------------------------------------------------------------
-- Nix flake operations

nix_flakeMetadata :: Text -> IO Value
nix_flakeMetadata flake = do
  out <- readProcessStdout_
    (proc "nix" ["flake", "metadata", "--json", Text.unpack flake])
  case Json.decode out of
    Nothing -> error "Nix.flakeMetadata: invalid JSON"
    Just v  -> pure v

nix_flakeUpdate :: Text -> IO ()
nix_flakeUpdate dir =
  runProcess_
    (setWorkingDir (Text.unpack dir) (proc "nix" ["flake", "update"]))

nix_flakeLock :: Text -> IO Value
nix_flakeLock dir = do
  contents <- L.readFile (Text.unpack dir <> "/flake.lock")
  case Json.decode contents of
    Nothing -> error "Nix.flakeLock: could not parse flake.lock"
    Just v  -> pure v

nix_flakeInputs :: Text -> IO (Map Text Text)
nix_flakeInputs dir = do
  v <- nix_flakeLock dir
  pure $ case v of
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
nix_checkFlakeOutputs dir = do
  (code, _out, err) <- readProcess
    (proc "nix" ["flake", "check", Text.unpack dir])
  pure $ case code of
    ExitSuccess   -> Right "flake check passed"
    ExitFailure _ -> Left (Text.decodeUtf8 (L.toStrict err))

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

nix_profileInstall :: StorePath -> IO ()
nix_profileInstall (StorePath p) =
  runProcess_ (proc "nix" ["profile", "install", Text.unpack p])

nix_profileRemove :: Text -> IO ()
nix_profileRemove name =
  runProcess_ (proc "nix" ["profile", "remove", Text.unpack name])

nix_profileList :: IO [StorePath]
nix_profileList = do
  out <- readProcessStdout_ (proc "nix" ["profile", "list", "--json"])
  case Json.decode out of
    Just (Json.Object top) ->
      case Map.lookup "elements" (KeyMap.toMapText top) of
        Just (Json.Array elems) ->
          pure $ concatMap extractStorePaths $ foldr (:) [] elems
        _ -> pure []
    _ -> pure []
  where
    extractStorePaths :: Json.Value -> [StorePath]
    extractStorePaths (Json.Object m) =
      case Map.lookup "storePaths" (KeyMap.toMapText m) of
        Just (Json.Array paths) ->
          [ StorePath t | Json.String t <- foldr (:) [] paths ]
        _ -> []
    extractStorePaths _ = []

nix_gcCollect :: IO ()
nix_gcCollect = runProcess_ (proc "nix-collect-garbage" [])

nix_gcRoots :: IO [StorePath]
nix_gcRoots = do
  out <- readProcessStdout_ (proc "nix-store" ["--gc", "--print-roots"])
  pure
    $ map (StorePath . Text.strip . takeLeft . Text.splitOn " -> ")
    $ filter (not . Text.null)
    $ Text.lines
    $ Text.decodeUtf8
    $ L.toStrict out
  where
    takeLeft (x:_) = x
    takeLeft []    = ""

nix_addRoot :: StorePath -> Text -> IO ()
nix_addRoot (StorePath p) rootPath =
  runProcess_
    (proc "nix-store"
      ["--add-root", Text.unpack rootPath, "--indirect",
       "--realise", Text.unpack p])

nix_optimiseStore :: IO ()
nix_optimiseStore = runProcess_ (proc "nix" ["store", "optimise"])

--------------------------------------------------------------------------------
-- Sops operations

sops_get :: Text -> Text -> IO Secret
sops_get file key = do
  out <- readProcessStdout_
    (proc "sops" ["--decrypt", "--output-type", "json", Text.unpack file])
  case Json.decode out of
    Just (Json.Object m) ->
      case Map.lookup key (KeyMap.toMapText m) of
        Just (Json.String v) -> pure (Secret v)
        _ -> error $ "Sops.get: key not found: " <> Text.unpack key
    _ -> error "Sops.get: sops decrypt failed"

sops_getAll :: Text -> IO (Map Text Secret)
sops_getAll file = do
  out <- readProcessStdout_
    (proc "sops" ["--decrypt", "--output-type", "json", Text.unpack file])
  case Json.decode out of
    Just (Json.Object m) ->
      pure $ Map.fromList
        [ (k, Secret v)
        | (k, Json.String v) <- Map.toList (KeyMap.toMapText m)
        ]
    _ -> error "Sops.getAll: sops decrypt failed"

--------------------------------------------------------------------------------
-- Age operations

age_encrypt :: Text -> Text -> IO ByteString
age_encrypt pubkey plaintext = do
  (code, out, err) <- readProcess
    (setStdin
      (byteStringInput (L.fromStrict (Text.encodeUtf8 plaintext)))
      (proc "age" ["--encrypt", "--recipient", Text.unpack pubkey]))
  case code of
    ExitSuccess   -> pure (L.toStrict out)
    ExitFailure _ ->
      error $ "Age.encrypt failed: "
           <> Text.unpack (Text.decodeUtf8 (L.toStrict err))

age_decrypt :: Text -> ByteString -> IO Secret
age_decrypt identityFile ciphertext = do
  (code, out, err) <- readProcess
    (setStdin
      (byteStringInput (L.fromStrict ciphertext))
      (proc "age" ["--decrypt", "--identity", Text.unpack identityFile]))
  case code of
    ExitSuccess   -> pure $ Secret $ Text.decodeUtf8 $ L.toStrict out
    ExitFailure _ ->
      error $ "Age.decrypt failed: "
           <> Text.unpack (Text.decodeUtf8 (L.toStrict err))

ssh_toAge :: Text -> IO Text
ssh_toAge pubkeyPath = do
  out <- readProcessStdout_
    (proc "ssh-to-age" ["-i", Text.unpack pubkeyPath])
  pure $ Text.strip $ Text.decodeUtf8 $ L.toStrict out

--------------------------------------------------------------------------------
-- Shell safety

shell_escape :: Text -> Text
shell_escape t = "'" <> Text.replace "'" "'\\''" t <> "'"

shell_escapeList :: [Text] -> Text
shell_escapeList ts = Text.intercalate " " (map shell_escape ts)

shell_which :: Text -> IO (Maybe StorePath)
shell_which cmd = do
  (code, out, _err) <- readProcess (proc "which" [Text.unpack cmd])
  pure $ case code of
    ExitSuccess ->
      Just $ StorePath $ Text.strip $ Text.decodeUtf8 $ L.toStrict out
    _ -> Nothing

shell_inPath :: Text -> IO Bool
shell_inPath cmd = do
  code <- runProcess (proc "which" [Text.unpack cmd])
  pure $ case code of
    ExitSuccess -> True
    _           -> False

--------------------------------------------------------------------------------
-- NixOS operations

nixos_rebuild :: Text -> IO ExitCode
nixos_rebuild action =
  runProcess (proc "nixos-rebuild" [Text.unpack action])

nixos_currentSystem :: IO StorePath
nixos_currentSystem = do
  out <- readProcessStdout_ (proc "readlink" ["-f", "/run/current-system"])
  pure $ StorePath $ Text.strip $ Text.decodeUtf8 $ L.toStrict out

-- | Evaluate a NixOS option via the flake-based system configuration.
-- Reads the hostname to select the right nixosConfigurations entry,
-- then calls nix eval --json on the resulting attribute path.
nixos_option :: Text -> IO Value
nixos_option option = do
  hostname <- fmap (Text.strip . Text.decodeUtf8 . L.toStrict)
                (readProcessStdout_ (proc "hostname" []))
  let expr = Text.unpack $ Text.concat
               [ "(builtins.getFlake \"/etc/nixos\")"
               , ".nixosConfigurations."
               , hostname
               , ".config."
               , option
               ]
  out <- readProcessStdout_
    (proc "nix" ["eval", "--json", "--impure", "--expr", expr])
  case Json.decode out of
    Nothing -> error $ "NixOS.option: invalid JSON for " <> Text.unpack option
    Just v  -> pure v

nixos_generations :: IO Text
nixos_generations = do
  entries <- Dir.listDirectory "/nix/var/nix/profiles"
  let gens = filter (Text.isPrefixOf "system-" . Text.pack) entries
  pure $ Text.unlines $ map Text.pack $ foldr (:) [] gens

nixos_rollback :: IO ()
nixos_rollback =
  runProcess_ (proc "nixos-rebuild" ["--rollback", "switch"])

--------------------------------------------------------------------------------
-- Systemd operations

systemd_status :: Text -> IO Text
systemd_status unit = do
  (_code, out, _err) <-
    readProcess (proc "systemctl" ["status", Text.unpack unit])
  pure $ Text.decodeUtf8 $ L.toStrict out

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
  out <- readProcessStdout_
    (proc "journalctl" ["-u", Text.unpack unit, "--no-pager"])
  pure $ Text.decodeUtf8 $ L.toStrict out
