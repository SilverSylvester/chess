--------------------------------------------------------------------
-- |
-- Module     : Data.Piece
-- Copyright  : (c) Conor Reynolds 2016
-- License    : MIT
-- Maintainer : reynolds.conor@gmail.com
-- Stability  : experimental
-- Portability: non-portable
--              Module responsible for generic piece operations.
--
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
east b = Board.notFileA .&. shiftL b 1

-- | Moves each component one position west
west :: Word64 -> Word64
west b = Board.notFileH .&. shiftR b 1

-- | Moves each component one position north-east
northEast :: Word64 -> Word64
northEast b = Board.notFileA .&. shiftL b 9

-- | Moves each component one position south-east
southEast :: Word64 -> Word64
southEast b = Board.notFileA .&. shiftR b 7

-- | Moves each component one position north-west
northWest :: Word64 -> Word64
northWest b = Board.notFileH .&. shiftL b 7

-- | Moves each component one position south-west
southWest :: Word64 -> Word64
southWest b = Board.notFileH .&. shiftR b 9

-- | Fills all spaces north of tagged positions
northFill :: Word64 -> Word64
northFill board =
    let iter0 = board
        iter1 = iter0 .|. shiftL iter0 8
        iter2 = iter1 .|. shiftL iter1 16
    in          iter2 .|. shiftL iter2 32

-- | Fills all spaces south of tagged positions
southFill :: Word64 -> Word64
southFill board =
    let iter0 = board
        iter1 = iter0 .|. shiftR iter0 8
        iter2 = iter1 .|. shiftR iter1 16
    in          iter2 .|. shiftR iter2 32

-- | Fills all spaces east of tagged positions
eastFill :: Word64 -> Word64
eastFill b =
    let pr0 = Board.notFileA
        pr1 = pr0 .&. shiftL pr0 1
        pr2 = pr1 .&. shiftL pr1 2
        it1 = b   .|. pr0 .&. shiftL b   1
        it2 = it1 .|. pr1 .&. shiftL it1 2
    in        it2 .|. pr2 .&. shiftL it2 4

-- | Fills all spaces west of tagged positions
westFill :: Word64 -> Word64
westFill b =
    let pr0 = Board.notFileH
        pr1 = pr0 .&. shiftR pr0 1
        pr2 = pr1 .&. shiftR pr1 2
        it1 = b   .|. pr0 .&. shiftR b   1
        it2 = it1 .|. pr1 .&. shiftR it1 2
    in        it2 .|. pr2 .&. shiftR it2 4

-- | Fills all spaces north-east of tagged positions
northEastFill :: Word64 -> Word64
northEastFill b =
    let pr0 = Board.notFileA
        pr1 = pr0 .&. shiftL pr0 9
        pr2 = pr1 .&. shiftL pr1 18
        it1 = b   .|. pr0 .&. shiftL b   9
        it2 = it1 .|. pr1 .&. shiftL it1 18
    in        it2 .|. pr2 .&. shiftL it2 36

-- | Fills all spaces north-west of tagged positions
northWestFill :: Word64 -> Word64
northWestFill b =
    let pr0 = Board.notFileH
        pr1 = pr0 .&. shiftL pr0 7
        pr2 = pr1 .&. shiftL pr1 14
        it1 = b   .|. pr0 .&. shiftL b   7
        it2 = it1 .|. pr1 .&. shiftL it1 14
    in        it2 .|. pr2 .&. shiftL it2 28

-- | Fills all spaces south-east of tagged positions
southEastFill :: Word64 -> Word64
southEastFill b =
    let pr0 = Board.notFileA
        pr1 = pr0 .&. shiftR pr0 7
        pr2 = pr1 .&. shiftR pr1 14
        it1 = b   .|. pr0 .&. shiftR b   7
        it2 = it1 .|. pr1 .&. shiftR it1 14
    in        it2 .|. pr2 .&. shiftR it2 28

-- | Fills all spaces south-west of tagged positions
southWestFill :: Word64 -> Word64
southWestFill b =
    let pr0 = Board.notFileH
        pr1 = pr0 .&. shiftR pr0 9
        pr2 = pr1 .&. shiftR pr1 18
        it1 = b   .|. pr0 .&. shiftR b   9
        it2 = it1 .|. pr1 .&. shiftR it1 18
    in        it2 .|. pr2 .&. shiftR it2 36

