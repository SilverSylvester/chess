--------------------------------------------------------------------
-- |
-- Module     : Data.Piece.Pawn
-- Copyright  : (c) Conor Reynolds 2016
-- License    : MIT
-- Maintainer : reynolds.conor@gmail.com
-- Stability  : experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Data.Piece.Pawn (
                       -- Do we really need *all* these functions?
                         fileFill
                       , northSpan
                       , southSpan
                       , interSpan
                       , northStopSquares
                       , southStopSquares
                       , eastAttackFileFill
                       , westAttackFileFill
                       , fullAttackFileFill
                       
                       -- ... especially these functions?
                       , eastAttackNorthSpanW
                       , westAttackNorthSpanW
                       , fullAttackNorthSpanW
                       , eastAttackSouthSpanW
                       , westAttackSouthSpanW
                       , fullAttackSouthSpanW
                       ) where

import Control.Applicative ((<$>),(<*>))

import Data.Bits
import qualified Data.Board as Board
import Data.Piece ( north
                  , south
                  , east
                  , west
                  , northFill
                  , southFill )
import Data.Word

----------------
-- PAWN FILLS --
----------------

-- | Union of north and south fills
fileFill :: Word64 -> Word64
fileFill = (.|.) <$> northFill <*> southFill

----------------
-- PAWN SPANS --
----------------

-- | Spaces north of the set of pawns on the board
northSpan :: Word64 -> Word64
northSpan = north . northFill

-- | Spaces south of the set of pawns on the board
southSpan :: Word64 -> Word64
southSpan = south . southFill

-- | Intersection of front and rear spans
interSpan :: Word64 -> Word64
interSpan = (.&.) <$> northSpan <*> southSpan

-- | Stop squares north of the set of pawns
northStopSquares :: Word64 -> Word64
northStopSquares = north

-- | Stop squares south of the set of pawns
southStopSquares :: Word64 -> Word64
southStopSquares = south

----------------------------------
-- COLOUR AGNOSTIC ATTACK SPANS --
----------------------------------

-- | East-side pawn attack file
eastAttackFileFill :: Word64 -> Word64
eastAttackFileFill = east . fileFill

-- | West-side pawn attack file
westAttackFileFill :: Word64 -> Word64
westAttackFileFill = west . fileFill

fullAttackFileFill :: Word64 -> Word64
fullAttackFileFill = (.|.) <$> westAttackFileFill
                           <*> eastAttackFileFill

------------------------
-- WHITE ATTACK SPANS --
------------------------

eastAttackNorthSpanW :: Word64 -> Word64
eastAttackNorthSpanW = east . northSpan

westAttackNorthSpanW :: Word64 -> Word64
westAttackNorthSpanW = west . northSpan

fullAttackNorthSpanW :: Word64 -> Word64
fullAttackNorthSpanW = (.|.) <$> westAttackNorthSpanW
                             <*> eastAttackNorthSpanW

eastAttackSouthSpanW :: Word64 -> Word64
eastAttackSouthSpanW = east . southFill

westAttackSouthSpanW :: Word64 -> Word64
westAttackSouthSpanW = west . southFill

fullAttackSouthSpanW :: Word64 -> Word64
fullAttackSouthSpanW = (.|.) <$> westAttackSouthSpanW
                             <*> eastAttackSouthSpanW

------------------------
-- BLACK ATTACK SPANS --
------------------------

eastAttackNorthSpanB :: Word64 -> Word64
eastAttackNorthSpanB = east . northFill

westAttackNorthSpanB :: Word64 -> Word64
westAttackNorthSpanB = west . northFill

fullAttackNorthSpanB :: Word64 -> Word64
fullAttackNorthSpanB = (.|.) <$> westAttackNorthSpanB
                             <*> eastAttackNorthSpanB

eastAttackSouthSpanB :: Word64 -> Word64
eastAttackSouthSpanB = east . southSpan

westAttackSouthSpanB :: Word64 -> Word64
westAttackSouthSpanB = west . southSpan

fullAttackSouthSpanB :: Word64 -> Word64
fullAttackSouthSpanB = (.|.) <$> westAttackSouthSpanB
                             <*> eastAttackSouthSpanB

