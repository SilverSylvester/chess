--------------------------------------------------------------------
-- |
-- Module     : Data.Piece.King
-- Copyright  : (c) Conor Reynolds 2016
-- License    : MIT
-- Maintainer : reynolds.conor@gmail.com
-- Stability  : experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Data.Piece.King ( kingAttacks ) where

import qualified Data.Board as Board
import Data.Piece ( north
                  , south
                  , east
                  , west )
import Data.Bits
import Data.Word

-- | Generates unobstructed attack squares for kings
kingAttacks :: Word64 -> Word64
kingAttacks b =
    let attacks = east b .|. west b
        kingSet = b .|. attacks
    in  attacks .|. north kingSet .|. south kingSet

