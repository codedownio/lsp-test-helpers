{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.LSP.Test.Notebook.Types
import Language.LSP.Test.Notebook.Hover

-- | Simple example demonstrating the LSP testing library
main :: IO ()
main = do
  putStrLn "lsp-test-notebook library built successfully!"
  putStrLn "Use the library functions in your tests!"