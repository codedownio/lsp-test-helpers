module Language.LSP.Test.Notebook.Diagnostics where

import Control.Lens ((^.))
import Control.Applicative (Alternative)
import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Trans (lift)
import Data.Aeson as A
import qualified Data.ByteString as B
import Data.Maybe
import Data.String.Interpolate
import Data.Text (Text)
import GHC.Int
import GHC.Stack
import Language.LSP.Protocol.Lens as LSP hiding (diagnostics)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Test.Sandwich as Sandwich
import UnliftIO.Async
import UnliftIO.Exception
import UnliftIO.IORef
import UnliftIO.STM

-- | Simple waitUntil implementation
waitUntil :: MonadIO m => Double -> m a -> m a
waitUntil _timeoutSeconds action = action -- Simplified for now

-- | Assert that diagnostics match expected ranges and codes
assertDiagnosticRanges :: (HasCallStack, MonadIO m) => [Diagnostic] -> [(Range, Maybe (Int32 |? Text))] -> m ()
assertDiagnosticRanges diagnostics desired = if
  | found == desired -> return ()
  | otherwise ->
      liftIO $ expectationFailure [__i|Got wrong diagnostics!

                              Expected: #{A.encode desired}

                              Found: #{A.encode found}
                             |]
  where
    found = getDiagnosticRanges diagnostics

-- | Extract diagnostic ranges and codes
getDiagnosticRanges :: [Diagnostic] -> [(Range, Maybe (Int32 |? Text))]
getDiagnosticRanges = fmap (\x -> (x ^. range, x ^. code))

-- | Assert diagnostics with ranges, codes, and messages
assertDiagnosticRanges' :: (HasCallStack, MonadIO m) => [Diagnostic] -> [(Range, Maybe (Int32 |? Text), Text)] -> m ()
assertDiagnosticRanges' diagnostics desired = if
  | found == desired -> return ()
  | otherwise ->
      liftIO $ expectationFailure [__i|Got wrong diagnostics!

                              Expected: #{A.encode desired}

                              Found: #{A.encode found}
                             |]
  where
    found = getDiagnosticRanges' diagnostics

-- | Extract diagnostic ranges, codes, and messages
getDiagnosticRanges' :: [Diagnostic] -> [(Range, Maybe (Int32 |? Text), Text)]
getDiagnosticRanges' = fmap (\x -> (x ^. range, x ^. code, x ^. LSP.message))

-- Diagnostics utilities re-exported from LSP.Test

-- | Filter diagnostics by severity
filterDiagnosticsBySeverity :: DiagnosticSeverity -> [Diagnostic] -> [Diagnostic]
filterDiagnosticsBySeverity severity = filter (\d -> d ^. LSP.severity == Just severity)

-- | Get error diagnostics only
getErrorDiagnostics :: [Diagnostic] -> [Diagnostic]
getErrorDiagnostics = filterDiagnosticsBySeverity DiagnosticSeverity_Error

-- | Get warning diagnostics only
getWarningDiagnostics :: [Diagnostic] -> [Diagnostic]
getWarningDiagnostics = filterDiagnosticsBySeverity DiagnosticSeverity_Warning

-- | Check if diagnostics contain any errors
hasErrors :: [Diagnostic] -> Bool
hasErrors = not . null . getErrorDiagnostics

-- | Check if diagnostics contain any warnings
hasWarnings :: [Diagnostic] -> Bool
hasWarnings = not . null . getWarningDiagnostics
