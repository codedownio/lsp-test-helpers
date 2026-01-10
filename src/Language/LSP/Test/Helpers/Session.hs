
module Language.LSP.Test.Helpers.Session where

import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Unlift
import qualified Data.ByteString as B
import Data.Default
import qualified Data.Map as M
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T hiding (filter)
import qualified Data.Text.IO as T
import Language.LSP.Protocol.Capabilities
import Language.LSP.Protocol.Lens as LSP hiding (diagnostics, hover, id, label, name, ranges)
import Language.LSP.Test
import Language.LSP.Test.Helpers.Types
import System.FilePath
import System.IO.Temp (createTempDirectory)
import Test.Sandwich as Sandwich
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.Process


withLspSession :: (
  LspContext ctx m
  ) => LspSessionOptions -> (FilePath -> Session (ExampleT ctx m) a) -> ExampleT ctx m a
withLspSession (LspSessionOptions {..}) session = do
  Just currentFolder <- getCurrentFolder

  homeDir <- liftIO $ createTempDirectory currentFolder "home"

  forM_ lspSessionOptionsExtraFiles $ \(path, bytes) -> do
    unless (isAbsolute path) $ do
      debug [i|Writing extra file: #{homeDir </> path}|]
      createDirectoryIfMissing True (homeDir </> takeDirectory path)
      liftIO $ B.writeFile (homeDir </> path) bytes

  createDirectoryIfMissing True (homeDir </> takeDirectory lspSessionOptionsInitialFileName)

  -- Comment this and use openDoc' to simulate an unsaved document
  liftIO $ T.writeFile (homeDir </> lspSessionOptionsInitialFileName) lspSessionOptionsInitialCode

  let sessionConfig = def { lspConfig = fromMaybe mempty (lspConfigInitializationOptions lspSessionOptionsConfig)
                          , logStdErr = True
                          , logMessages = True
                          , messageTimeout = 120
                          }

  let cmd:args = fmap T.unpack $ lspConfigArgs lspSessionOptionsConfig
  (cp, modifyCp) <- getContext maybeBubblewrap >>= \case
    Nothing -> do
      let configEnv = maybe mempty (fmap (bimap T.unpack T.unpack) . M.toList) (lspConfigEnv lspSessionOptionsConfig)
      let finalEnv = ("HOME", homeDir) : ("PATH", lspSessionOptionsPathEnvVar) : configEnv
      info [i|Language server environment: #{finalEnv}|]
      let modifyCp cp = cp { env = Just finalEnv
                           , cwd = Just homeDir }
      return (proc cmd args, modifyCp)
    Just bwrapBinary -> do
      let bwrapArgs = ["--tmpfs", "/tmp"
                      , "--bind", homeDir, homeDir
                      , "--clearenv"
                      , "--setenv", "HOME", homeDir
                      , "--chdir", homeDir

                      , "--setenv", "PATH", lspSessionOptionsPathEnvVar

                      , "--proc", "/proc"
                      , "--dev", "/dev"
                      ]
                      <> mconcat [["--ro-bind", x, x] | x <- lspSessionOptionsReadOnlyBinds]
                      <> mconcat [["--setenv", T.unpack n, T.unpack v]
                                 | (n, v) <- M.toList (fromMaybe mempty (lspConfigEnv lspSessionOptionsConfig))]
                      <> ["--"]
                      <> (cmd : args)

      return (proc bwrapBinary bwrapArgs, id)

  info [i|LSP command: #{cp}|]

  -- We don't support certain server-to-client requests, since the waitForDiagnostics doesn't handle them
  let caps = fullClientCapsForVersion (LSPVersion 3 16)
           & set (workspace . _Just . workspaceFolders) Nothing
           & set (workspace . _Just . configuration) Nothing
           & set (workspace . _Just . didChangeWatchedFiles . _Just . dynamicRegistration) (Just False)
           & set (workspace . _Just . didChangeConfiguration . _Just . dynamicRegistration) (Just False)
           & set (textDocument . _Just . semanticTokens . _Just . dynamicRegistration) (Just False)

  runSessionWithConfigCustomProcess modifyCp sessionConfig cp caps homeDir (session homeDir)

handleSessionException :: MonadUnliftIO m => ExampleT ctx m a -> ExampleT ctx m a
handleSessionException = handle (\(e :: SessionException) -> expectationFailure [i|LSP session failed with SessionException: #{e}|])
