{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Language.LSP.Test.Helpers.Hover where

import Control.Applicative (Alternative)
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack
import Language.LSP.Protocol.Lens as LSP hiding (hover)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Language.LSP.Test.Helpers.Readiness
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits


-- | Extract all text from hover contents
allHoverText :: Hover -> Text
allHoverText hover = allHoverContentsText (hover ^. contents)

-- | Type alias for hover contents (compatibility with different LSP versions)
type HoverContents = MarkupContent |? (MarkedString |? [MarkedString])

-- | Extract text from hover contents regardless of format
allHoverContentsText :: HoverContents -> Text
allHoverContentsText (InL (MarkupContent _ t)) = t
allHoverContentsText (InR markedStrings) = case markedStrings of
  InL ms -> markedStringToText ms
  InR mss -> mconcat $ fmap markedStringToText mss
  where
    markedStringToText (MarkedString (InL t)) = t
    markedStringToText (MarkedString (InR thing)) = thing ^. LSP.value

-- | Assert that hover text contains all specified substrings
hoverShouldContainAll :: MonadIO m => Hover -> [Text] -> m ()
hoverShouldContainAll hover expectedTexts = do
  let hoverText = allHoverText hover
  forM_ expectedTexts $ \expected ->
    hoverText `shouldContainText` expected

-- | Assert that hover text contains any of the specified substrings
hoverShouldContainAny :: MonadIO m => Hover -> [Text] -> m ()
hoverShouldContainAny hover expectedTexts = do
  let hoverText = allHoverText hover
  unless (any (`T.isInfixOf` hoverText) expectedTexts) $
    liftIO $ expectationFailure [i|Hover text '#{hoverText}' did not contain any of: #{expectedTexts}|]

-- | Check if text contains all specified substrings
containsAll :: Text -> [Text] -> Bool
containsAll haystack = all (`T.isInfixOf` haystack)

-- Session-level hover testing removed for now
-- Use getHover from Language.LSP.Test directly in your test sessions

-- | Helper function for text assertion
shouldContainText :: MonadIO m => Text -> Text -> m ()
shouldContainText haystack needle =
  unless (needle `T.isInfixOf` haystack) $
    liftIO $ expectationFailure [i|Expected text to contain '#{needle}', but got: '#{haystack}'|]

getHoverOrException :: (
  MonadLoggerIO m, MonadThrow m, MonadUnliftIO m, Alternative m
  ) => TextDocumentIdentifier -> Position -> Session m Hover
getHoverOrException tdi pos = getHover tdi pos >>= \case
  Nothing -> expectationFailure [i|No hover returned.|]
  Just x -> return x

-- | 'getHoverOrException', but first wait for the server to finish its
-- background work and then keep asking until it answers.
--
-- Use this for positions whose hover depends on more than the open file --
-- anything from a dependency or another module — since those come back empty
-- until the server has indexed the project. See 'waitForServerReady' for what
-- "ready" means and what it does not.
getHoverEventually :: (
  HasCallStack, MonadLoggerIO m, MonadThrow m, MonadUnliftIO m, Alternative m
  ) => TextDocumentIdentifier -> Position -> Session m Hover
getHoverEventually = getHoverEventually' defaultReadyTimeout

-- | 'getHoverEventually' with an explicit timeout in seconds, covering the
-- readiness wait and the retries separately.
getHoverEventually' :: (
  HasCallStack, MonadLoggerIO m, MonadThrow m, MonadUnliftIO m, Alternative m
  ) => Double -> TextDocumentIdentifier -> Position -> Session m Hover
getHoverEventually' timeoutSeconds tdi pos = do
  waitForServerReady' timeoutSeconds defaultSettleSeconds
  waitUntil timeoutSeconds $ getHoverOrException tdi pos
