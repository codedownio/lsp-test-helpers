module Language.LSP.Test.Notebook.Diagnostics where

import Control.Monad
import Data.Function (fix)
import Data.String.Interpolate
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Language.LSP.Test.Notebook.Session
import Language.LSP.Test.Notebook.Types
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits
import UnliftIO.Async
import UnliftIO.STM


-- testDiagnostics :: (
--   LspContext ctx m, HasNixEnvironment ctx
--   ) => Text -> FilePath -> Maybe LanguageKind -> Text -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
-- testDiagnostics name filename maybeLanguageId codeToTest = testDiagnostics' name filename maybeLanguageId codeToTest []

-- testDiagnostics' :: (
--   LspContext ctx m, HasNixEnvironment ctx
--   ) => Text -> FilePath -> Maybe LanguageKind -> Text -> [(FilePath, B.ByteString)] -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
-- testDiagnostics' name filename maybeLanguageId codeToTest = testDiagnostics'' [i|#{name}, #{filename} with #{show codeToTest} (diagnostics)|] name filename maybeLanguageId codeToTest

-- testDiagnosticsLabel :: (
--   LspContext ctx m, HasNixEnvironment ctx
--   ) => String -> Text -> FilePath -> Maybe LanguageKind -> Text -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
-- testDiagnosticsLabel label name filename maybeLanguageId codeToTest = testDiagnostics'' label name filename maybeLanguageId codeToTest []

testDiagnosticsLabelDesired :: (
  LspContext ctx m
  ) => LspSessionOptions -> String -> ([Diagnostic] -> Bool) -> SpecFree ctx m ()
testDiagnosticsLabelDesired lspSessionOptions label cb = it label $
  testDiagnostics lspSessionOptions $ \diags ->
    if | cb diags -> return True
       | otherwise -> expectationFailure [i|Got unexpected diagnostics: #{diags}|]

testDiagnostics :: (
  LspContext ctx m
  ) => LspSessionOptions -> ([Diagnostic] -> Session (ExampleT ctx m) Bool) -> ExampleT ctx m ()
testDiagnostics = testDiagnostics' 60.0

testDiagnostics' :: (
  LspContext ctx m
  ) => Double -> LspSessionOptions -> ([Diagnostic] -> Session (ExampleT ctx m) Bool) -> ExampleT ctx m ()
testDiagnostics' timeoutSeconds lspSessionOptions cb = do
  withLspSession lspSessionOptions $ \_homeDir -> do
    lastSeenDiagsVar <- newTVarIO mempty

    let watchDiagnostics = forever $ do
          diags <- waitForDiagnostics
          atomically $ writeTVar lastSeenDiagsVar diags

    withAsync watchDiagnostics $ \_ -> do
      waitUntil timeoutSeconds $ do
        flip fix [] $ \loop lastValue ->
          cb lastValue >>= \case
            True -> return ()
            False -> do
              newDiags <- atomically $ do
                x <- readTVar lastSeenDiagsVar
                when (x == lastValue) retrySTM
                return x
              loop newDiags
