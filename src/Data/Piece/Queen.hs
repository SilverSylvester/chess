--------------------------------------------------------------------
-- |
-- Module     : Data.Piece.Queen
-- Copyright  : (c) Conor Reynolds 2016
-- License    : MIT
-- Maintainer : reynolds.conor@gmail.com
-- Stability  : experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Data.Piece.Queen ( queenAttacks ) where

import Data.Bits
import qualified Data.Board as Board
import Data.Piece ( northFill
                  , southFill
                  , eastFill
                  , westFill
                  , northEastFill
                  , northWestFill
                  , southEastFill
                  , southWestFill )
import Data.Word

-- | Generates unobstructed attack squares for queens
queenAttacks :: Word64 -> Word64
queenAttacks =
    (.|.) <$> ((.|.) <$> ((.|.) <$> northFill     <*> southFill)
                     <*> ((.|.) <$> eastFill      <*> westFill))
          <*> ((.|.) <$> ((.|.) <$> northWestFill <*> northEastFill)
                     <*> ((.|.) <$> southWestFill <*> southEastFill))

