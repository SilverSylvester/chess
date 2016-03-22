--------------------------------------------------------------------
-- |
-- Module     : Data.Piece.Knight
-- Copyright  : (c) Conor Reynolds 2016
-- License    : MIT
-- Maintainer : reynolds.conor@gmail.com
-- Stability  : experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Data.Piece.Knight where

import Control.Applicative ((<$>),(<*>))
import Data.Bits
import qualified Data.Board as Board
import Data.Piece
import Data.Word

-- | Generates the squares possible for a knight to
--   attack. Probably a little inefficient.
knightAttacks :: Word64 -> Word64
knightAttacks b = foldl (.|.) 0 $ map ($ b)
    [ north . north . east
    , north . north . west
    , north . east  . east
    , north . west  . west 
    , south . south . east
    , south . south . west
    , south . east  . east
    , south . west  . west ]

-- | Does the same thing as 'knightAttacks' but
--   retains the original squares the knights were
--   sitting on.
knightFill :: Word64 -> Word64
knightFill b = b .|. knightAttacks b

