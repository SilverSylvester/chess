--------------------------------------------------------------------
-- |
-- Module     :  Main
-- Copyright  :  (c) Conor Reynolds 2016
-- License    :  MIT
-- Maintainer :  reynolds.conor@gmail.com
-- Stability  :  experimental
-- Portability:  non-portable
--
--------------------------------------------------------------------

module Main where

import Control.Monad

import Data.Board        as Board
import Data.Board.Eval   as Eval
import Data.Piece        as Piece
import Data.Piece.Pawn   as Pawn
import Data.Piece.Knight as Knight
import Data.Piece.Bishop as Bishop
import Data.Piece.Rook   as Rook
import Data.Piece.Queen  as Queen
import Data.Piece.King   as King

-- | TODO: Make some sort of CLI interactive
--   chess board.
main = forever $ do
    print startingBoard
    getChar

