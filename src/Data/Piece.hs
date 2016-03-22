--------------------------------------------------------------------
-- |
-- Module     : Data.Piece
-- Copyright  : (c) Conor Reynolds 2016
-- License    : MIT
-- Maintainer : reynolds.conor@gmail.com
-- Stability  : experimental
-- Portability: non-portable
-- Description: Module responsible for generic piece operations
--------------------------------------------------------------------

module Data.Piece where

import Data.Bits
import qualified Data.Board as Board
import Data.Word

-- | Moves each component one position north
north :: Word64 -> Word64
north = flip shiftL 8

-- | Moves each component one position south
south :: Word64 -> Word64
south = flip shiftR 8

-- | Moves each component one position east
east :: Word64 -> Word64
east b = Board.notFileA .&. shiftR b 1

-- | Moves each component one position west
west :: Word64 -> Word64
west b = Board.notFileH .&. shiftL b 1

-- | Moves each component one position north-east
northEast :: Word64 -> Word64
northEast b = Board.notFileA .&. shiftR b 9

-- | Moves each component one position south-east
southEast :: Word64 -> Word64
southEast b = Board.notFileA .&. shiftL b 7

-- | Moves each component one position north-west
northWest :: Word64 -> Word64
northWest b = Board.notFileH .&. shiftR b 7

-- | Moves each component one position south-west
southWest :: Word64 -> Word64
southWest b = Board.notFileH .&. shiftL b 9

