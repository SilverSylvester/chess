--------------------------------------------------------------------
-- |
-- Module     : Data.Piece.Rook
-- Copyright  : (c) Conor Reynolds 2016
-- License    : MIT
-- Maintainer : reynolds.conor@gmail.com
-- Stability  : experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Data.Piece.Rook ( rookAttacks ) where

import Control.Applicative ((<$>),(<*>))

import Data.Bits
import qualified Data.Board as Board
import Data.Piece ( northFill
                  , southFill 
                  , eastFill
                  , westFill )
import Data.Word

-- | Generates unobstructed attack squares for rooks
rookAttacks :: Word64 -> Word64
rookAttacks = (.|.) <$> ((.|.) <$> northFill <*> southFill)
                    <*> ((.|.) <$> eastFill  <*> westFill)

