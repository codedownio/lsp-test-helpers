module Language.LSP.Test.Helpers.Readiness (
  waitForServerReady
  , waitForServerReady'
  , defaultReadyTimeout
  , defaultSettleSeconds
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import qualified Data.Set as Set
import GHC.Stack
import Language.LSP.Test
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits
import UnliftIO.Concurrent (threadDelay)


defaultReadyTimeout :: Double
defaultReadyTimeout = 60.0

defaultSettleSeconds :: Double
defaultSettleSeconds = 2.0

-- | Wait until the server has finished the background work it announced.
--
-- Servers report long-running work through the LSP's work-done progress
-- notifications: @$/progress@ carrying @begin@ and @end@ values, established in
-- LSP 3.15. This is protocol, not a server-specific extension, so any server
-- that reports progress at all works here. @lsp-test@ tracks which tokens have
-- begun but not ended, and 'getIncompleteProgressSessions' exposes that set.
--
-- A request that depends on project-wide knowledge — hovering an identifier
-- from a dependency, workspace symbols, cross-module go-to-definition — can
-- come back empty until that work finishes, so waiting for the set to drain
-- turns a race into a wait.
--
-- Two caveats worth knowing before relying on this:
--
-- * A server that never reports progress leaves the set empty throughout, and
--   this returns after the settle window. That makes it safe to call
--   unconditionally, but it means "ready" is only as good as the server's
--   progress reporting.
--
-- * The set is also empty in the moment before the server begins its first
--   progress session, so this waits @settleSeconds@ before each check rather
--   than trusting an immediately-empty set. That is a heuristic, not a
--   guarantee, which is why callers that can retry should still retry.
waitForServerReady :: (HasCallStack, MonadUnliftIO m) => Session m ()
waitForServerReady = waitForServerReady' defaultReadyTimeout defaultSettleSeconds

-- | 'waitForServerReady' with an explicit overall timeout and settle window,
-- both in seconds.
waitForServerReady' :: (HasCallStack, MonadUnliftIO m) => Double -> Double -> Session m ()
waitForServerReady' timeoutSeconds settleSeconds = waitUntil timeoutSeconds $ do
  threadDelay (round (settleSeconds * 1_000_000))
  outstanding <- getIncompleteProgressSessions
  unless (Set.null outstanding) $
    Sandwich.expectationFailure [i|Server still has work in progress: #{Set.toList outstanding}|]
