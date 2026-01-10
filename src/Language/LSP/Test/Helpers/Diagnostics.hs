
module Language.LSP.Test.Helpers.Diagnostics where

import Control.Lens
import Control.Monad
import Data.Maybe
import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Lens as LSP
import Language.LSP.Protocol.Types as LSP
import Language.LSP.Test
import Language.LSP.Test.Helpers.Session
import Language.LSP.Test.Helpers.Types
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
testDiagnosticsLabelDesired lspSessionOptions label' cb = it label' $
  testDiagnostics lspSessionOptions $ \diags ->
    if | cb diags -> return ()
       | otherwise -> expectationFailure [i|Got unexpected diagnostics: #{diags}|]

testDiagnostics :: (
  LspContext ctx m
  ) => LspSessionOptions -> ([Diagnostic] -> Session (ExampleT ctx m) ()) -> ExampleT ctx m ()
testDiagnostics = testDiagnostics' 60.0

testDiagnostics' :: (
  LspContext ctx m
  ) => Double -> LspSessionOptions -> ([Diagnostic] -> Session (ExampleT ctx m) ()) -> ExampleT ctx m ()
testDiagnostics' timeoutSeconds lspSessionOptions@(LspSessionOptions {..}) cb = do
  withLspSession lspSessionOptions $ \_homeDir -> do
    _ <- openDoc lspSessionOptionsInitialFileName (fromMaybe (LanguageKind_Custom "unknown") (lspConfigLanguageId lspSessionOptionsConfig))

    diagsChan <- newTChanIO

    let watchDiagnostics = forever $ do
          diags <- waitForDiagnostics
          info [i|waitForDiagnostics result: #{diags}|]
          atomically $ writeTChan diagsChan diags

    withAsync watchDiagnostics $ \_ -> do
      waitUntil timeoutSeconds $ do
        atomically (readTChan diagsChan) >>= cb

getDiagnosticRanges :: [Diagnostic] -> [(Range, Maybe (Int32 |? Text))]
getDiagnosticRanges = fmap (\x -> (x ^. range, x ^. code))

getDiagnosticRanges' :: [Diagnostic] -> [(Range, Maybe (Int32 |? Text), Text)]
getDiagnosticRanges' = fmap (\x -> (x ^. range, x ^. code, x ^. LSP.message))
