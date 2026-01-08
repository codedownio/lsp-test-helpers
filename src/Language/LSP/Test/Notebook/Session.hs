{-# LANGUAGE NumericUnderscores #-}

module Language.LSP.Test.Notebook.Session where

import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Aeson as A
import qualified Data.ByteString as B
import Data.Default
import Data.Function
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Language.LSP.Protocol.Lens as LSP hiding (id, name)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Language.LSP.Test.Notebook.Types
import System.FilePath
import System.IO.Temp (createTempDirectory)
import Test.Sandwich as Sandwich
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.IO
import UnliftIO.Process
import System.Environment (lookupEnv)

-- LSP session functions removed for now due to type compatibility issues
-- Users should use the functions from codedown-languages TestLib.LSP directly

-- | Get system PATH for language server execution
getSystemPath :: MonadIO m => m String
getSystemPath = liftIO $ do
  maybePath <- lookupEnv "PATH"
  case maybePath of
    Just path -> return path
    Nothing -> return "/usr/local/bin:/usr/bin:/bin"

-- | Create a simple compile_commands.json for C++ projects
createCompileCommands :: MonadIO m => FilePath -> FilePath -> m ()
createCompileCommands homeDir filename = liftIO $ do
  let compileCommands = A.object 
        [ "directory" A..= homeDir
        , "command" A..= ("clang++ -std=c++17 " ++ filename :: String)
        , "file" A..= (homeDir </> filename)
        ]
  A.encodeFile (homeDir </> "compile_commands.json") [compileCommands]
