{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module NixHell
  ( -- Types
    StorePath(..)
  , Secret(..)
    -- StorePath
  , storePath_fromText
  , storePath_toText
    -- Secret
  , secret_toEnvValue
  , secret_writeFile
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
    -- Profile and GC
  , nix_profileInstall
  , nix_profileRemove
  , nix_gcCollect
  , nix_gcRoots
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
import qualified System.IO as IO
import System.Exit (ExitCode(..))
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
-- into display, logging, or string concatenation. The only extraction
-- points are secret_toEnvValue and secret_writeFile.
newtype Secret = Secret Text

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

-- | The only way to extract a secret into plain Text. The verbose
-- name makes every extraction visible during code review.
secret_toEnvValue :: Secret -> Text
secret_toEnvValue (Secret t) = t

-- | Write a secret to a file with mode 600.
secret_writeFile :: Text -> Secret -> IO ()
secret_writeFile path (Secret t) = do
  ByteString.writeFile (Text.unpack path) (Text.encodeUtf8 t)
  runProcess_ (proc "chmod" ["600", Text.unpack path])

--------------------------------------------------------------------------------
-- Nix store operations

nix_build :: Text -> IO StorePath
nix_build attr = do
  out <- readProcessStdout_
    (proc "nix" ["build", "--no-link", "--print-out-paths", Text.unpack attr])
  pure $ StorePath $ Text.strip $ Text.decodeUtf8 $ L.toStrict out

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

nix_instantiate :: Text -> IO StorePath
nix_instantiate expr = do
  out <- readProcessStdout_
    (proc "nix-instantiate" ["--expr", Text.unpack expr])
  pure $ StorePath $ Text.strip $ Text.decodeUtf8 $ L.toStrict out

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
            [ (k, url)
            | (k, Json.Object node) <- Map.toList (KeyMap.toMapText nodes)
            , k /= "root"
            , Just (Json.Object locked) <-
                [Map.lookup "locked" (KeyMap.toMapText node)]
            , Just (Json.String url) <-
                [Map.lookup "url" (KeyMap.toMapText locked)]
            ]
        _ -> Map.empty
    _ -> Map.empty

--------------------------------------------------------------------------------
-- Profile and GC

nix_profileInstall :: StorePath -> IO ()
nix_profileInstall (StorePath p) =
  runProcess_ (proc "nix" ["profile", "install", Text.unpack p])

nix_profileRemove :: Text -> IO ()
nix_profileRemove name =
  runProcess_ (proc "nix" ["profile", "remove", Text.unpack name])

nix_gcCollect :: IO ()
nix_gcCollect = runProcess_ (proc "nix-collect-garbage" [])

nix_gcRoots :: IO [StorePath]
nix_gcRoots = do
  out <- readProcessStdout_ (proc "nix-store" ["--gc", "--print-roots"])
  pure
    $ map (StorePath . Text.strip . head . Text.splitOn " -> ")
    $ filter (not . Text.null)
    $ Text.lines
    $ Text.decodeUtf8
    $ L.toStrict out

nix_optimiseStore :: IO ()
nix_optimiseStore = runProcess_ (proc "nix" ["store", "optimise"])

--------------------------------------------------------------------------------
-- Secret operations

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

age_decrypt :: ByteString -> IO Secret
age_decrypt ciphertext = do
  (code, out, err) <- readProcess
    (setStdin
      (byteStringInput (L.fromStrict ciphertext))
      (proc "age" ["--decrypt"]))
  case code of
    ExitSuccess   ->
      pure $ Secret $ Text.decodeUtf8 $ L.toStrict out
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

nixos_option :: Text -> IO Value
nixos_option option = do
  out <- readProcessStdout_
    (proc "nixos-option" ["--json", Text.unpack option])
  case Json.decode out of
    Nothing -> error "NixOS.option: invalid JSON"
    Just v  -> pure v

nixos_generations :: IO Text
nixos_generations = do
  out <- readProcessStdout_
    (proc "nix-env"
      ["--list-generations", "--profile", "/nix/var/nix/profiles/system"])
  pure $ Text.decodeUtf8 $ L.toStrict out

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