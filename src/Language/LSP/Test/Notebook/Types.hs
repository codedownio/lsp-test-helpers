{-# LANGUAGE OverloadedStrings #-}
module Language.LSP.Test.Notebook.Types where

import Control.Applicative (Alternative)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson as A
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Protocol.Types
import Test.Sandwich as Sandwich
import UnliftIO.Exception

-- | Language server configuration
data LanguageServerConfig = LanguageServerConfig {
  lspConfigName :: Text,                      -- ^ Language server executable name
  lspConfigArgs :: [Text],                    -- ^ Command line arguments
  lspConfigLanguageId :: Maybe LanguageKind,  -- ^ LSP language identifier
  lspConfigNotebookSuffix :: Text,           -- ^ File extension for notebooks
  lspConfigInitializationOptions :: Maybe A.Object, -- ^ LSP initialization options
  lspConfigEnv :: Maybe (Map Text Text)      -- ^ Additional environment variables
}


-- | Type constraint for LSP testing context
-- Note: Simplified version without context requirements from codedown-languages
type LspTestContext ctx m = (
  Alternative m,
  MonadIO m,
  MonadBaseControl IO m,
  MonadUnliftIO m,
  MonadCatch m,
  MonadThrow m,
  MonadMask m
  )
