module Language.LSP.Test.Notebook.Util where

import Control.Monad (when, unless)
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Protocol.Types
import Test.Sandwich as Sandwich

-- | Assert that text contains a substring
textShouldContain :: MonadIO m => Text -> Text -> m ()
textShouldContain haystack needle = 
  unless (needle `T.isInfixOf` haystack) $
    liftIO $ expectationFailure [i|Expected text to contain '#{needle}', but got: '#{haystack}'|]

-- | Assert that text does not contain a substring  
textShouldNotContain :: MonadIO m => Text -> Text -> m ()
textShouldNotContain haystack needle = 
  when (needle `T.isInfixOf` haystack) $
    liftIO $ expectationFailure [i|Expected text not to contain '#{needle}', but it did: '#{haystack}'|]

-- | Assert that two positions are equal
positionShouldBe :: MonadIO m => Position -> Position -> m ()
positionShouldBe actual expected = 
  unless (actual == expected) $
    liftIO $ expectationFailure [i|Expected position #{expected}, but got #{actual}|]

-- | Assert that a range contains a position
rangeShouldContain :: MonadIO m => Range -> Position -> m ()
rangeShouldContain range pos = 
  unless (isPositionInRange pos range) $
    liftIO $ expectationFailure [i|Expected range #{range} to contain position #{pos}|]

-- | Check if position is within range
isPositionInRange :: Position -> Range -> Bool
isPositionInRange (Position line char) (Range (Position startLine startChar) (Position endLine endChar)) =
  (line > startLine || (line == startLine && char >= startChar)) &&
  (line < endLine || (line == endLine && char <= endChar))

-- | Create a position
mkPosition :: Int -> Int -> Position
mkPosition line char = Position (fromIntegral line) (fromIntegral char)

-- | Create a range
mkRange :: Int -> Int -> Int -> Int -> Range  
mkRange startLine startChar endLine endChar = 
  Range (mkPosition startLine startChar) (mkPosition endLine endChar)

-- | Extract line and character as Int from Position
positionToInts :: Position -> (Int, Int)
positionToInts (Position line char) = (fromIntegral line, fromIntegral char)

-- | Extract range bounds as Ints
rangeToInts :: Range -> (Int, Int, Int, Int)
rangeToInts (Range (Position startLine startChar) (Position endLine endChar)) = 
  (fromIntegral startLine, fromIntegral startChar, fromIntegral endLine, fromIntegral endChar)
