--------------------------------------------------------------------
-- |
-- Module     : Data.Piece.Bishop
-- Copyright  : (c) Conor Reynolds 2016
-- License    : MIT
-- Maintainer : reynolds.conor@gmail.com
-- Stability  : experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Data.Piece.Bishop ( bishopAttacks ) where

import Control.Applicative ((<$>),(<*>))

import Data.Bits
import qualified Data.Board as Board
import Data.Piece ( northEastFill
                  , northWestFill
                  , southEastFill
                  , southWestFill )
import Data.Word

-- | Generates unobstructed attack squares for bishops
bishopAttacks :: Word64 -> Word64
bishopAttacks = (.|.) <$> ((.|.) <$> northWestFill <*> northEastFill)
                      <*> ((.|.) <$> southWestFill <*> southEastFill)

