{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.LSP.Test.Helpers.Types where

import Control.Applicative (Alternative)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson as A
import Data.Aeson.TH as A
import qualified Data.ByteString as B
import Data.Map (Map)
import qualified Data.Set as S
import Data.Text (Text)
import Language.LSP.Protocol.Types
import Language.LSP.Test (SessionConfig)
import Language.LSP.Test.Helpers.Util.Aeson
import Test.Sandwich as Sandwich


data LanguageServerType = LanguageServerTypeTcp
                        | LanguageServerTypeStream
  deriving (Show, Eq)
deriveJSON toSnakeC3 ''LanguageServerType

data LanguageServerConfig = LanguageServerConfig {
  lspConfigName :: Text
  , lspConfigVersion :: Maybe Text
  , lspConfigDescription :: Maybe Text
  , lspConfigDisplayName :: Maybe Text
  , lspConfigIcon :: Maybe FilePath
  , lspConfigExtensions :: [Text]
  , lspConfigAttrs :: S.Set Text
  , lspConfigType :: LanguageServerType
  , lspConfigPrimary :: Maybe Bool
  , lspConfigArgs :: [Text]
  , lspConfigLanguageId :: Maybe LanguageKind
  , lspConfigInitializationOptions :: Maybe A.Object
  , lspConfigNotebookSuffix :: Text
  , lspConfigKernelName :: Maybe Text
  , lspConfigEnv :: Maybe (Map Text Text)
  , lspConfigFile :: Maybe FilePath
  , lspConfigIsBuiltIn :: Maybe Bool
  } deriving (Show, Eq)
deriveJSON toSnake2 ''LanguageServerConfig

maybeBubblewrap :: Label "maybeBubblewrap" (Maybe FilePath)
maybeBubblewrap = Label
type HasMaybeBubblewrap context = HasLabel context "maybeBubblewrap" (Maybe FilePath)

type LspContext ctx m = (
  Alternative m
  , MonadIO m
  , MonadBaseControl IO m
  , MonadUnliftIO m
  , MonadCatch m
  , MonadThrow m
  , MonadMask m

  , HasBaseContext ctx
  , HasMaybeBubblewrap ctx
  )

data LspSessionOptions = LspSessionOptions {
  lspSessionOptionsConfig :: LanguageServerConfig
  -- | Value of the PATH environment variable for the language server.
  -- Applies to either bwrap or non-bwrap setups.
  , lspSessionOptionsPathEnvVar :: String
  -- | Read-only binds (bwrap setups only).
  , lspSessionOptionsReadOnlyBinds :: [FilePath]
  -- | Read-only binds (bwrap setups only).
  , lspSessionOptionsModifySessionConfig :: SessionConfig -> SessionConfig

  -- | Initial filename to open.
  , lspSessionOptionsInitialFileName :: FilePath
  -- | Initial filename to open.
  , lspSessionOptionsInitialLanguageKind :: LanguageKind
  -- | Contents of the initial file.
  , lspSessionOptionsInitialCode :: Text

  -- | Extra files to place in the home directory.
  , lspSessionOptionsExtraFiles :: [(FilePath, B.ByteString)]
  }

defaultLspSessionOptions :: LanguageServerConfig -> LspSessionOptions
defaultLspSessionOptions config = LspSessionOptions {
  lspSessionOptionsConfig = config
  , lspSessionOptionsPathEnvVar = ""
  , lspSessionOptionsReadOnlyBinds = []
  , lspSessionOptionsModifySessionConfig = id

  , lspSessionOptionsInitialFileName = "test.py"
  , lspSessionOptionsInitialLanguageKind = LanguageKind_Custom "unknown"
  , lspSessionOptionsInitialCode = ""

  , lspSessionOptionsExtraFiles = []
  }

data LspSessionInfo = LspSessionInfo {
  lspSessionInfoHomeDir :: FilePath
  , lspSessionInfoFileName :: FilePath
  }
