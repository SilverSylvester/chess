--------------------------------------------------------------------
-- |
-- Module     :  Data.Board.Eval
-- Copyright  :  (c) Conor Reynolds 2016
-- License    :  MIT
-- Maintainer :  reynolds.conor@gmail.com
-- Stability  :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Data.Board.Eval where

import Data.Bits
import Data.Board
import Data.Word

import           Data.Piece
import qualified Data.Piece.Pawn   as Pawn
import qualified Data.Piece.Knight as Knight
import qualified Data.Piece.Bishop as Bishop
import qualified Data.Piece.Rook   as Rook
import qualified Data.Piece.Queen  as Queen
import qualified Data.Piece.King   as King

type Score = Double

-- | Positive numbers are good for white.
--   Negative numbers are good for black.
--   (map (fromIntegral .) ?)
--   
--   Something *has* to be done to shorten
--   this function.
evalBoard :: Board -> Score
evalBoard b = 200 * (wkc  b - bkc  b)
            +   9 * (wqc  b - bqc  b)
            +   5 * (wrc  b - brc  b)
            +   3 * (wbc  b - bbc  b)
            +   3 * (wnc  b - bnc  b)
            +   1 * (wpc  b - bpc  b)
            - 0.5 * (wdpc b - bdpc b)
            - 0.5 * (wipc b - bipc b)
            - 0.5 * (wbpc b - bbpc b)
            + 0.1 * (wlmc b - blmc b)
  
  where wkc  = fromIntegral . whiteKingCount
        bkc  = fromIntegral . blackKingCount
        wqc  = fromIntegral . whiteQueenCount
        bqc  = fromIntegral . blackQueenCount
        wrc  = fromIntegral . whiteRookCount
        brc  = fromIntegral . blackRookCount
        wbc  = fromIntegral . whiteBishopCount
        bbc  = fromIntegral . blackBishopCount
        wnc  = fromIntegral . whiteKnightCount
        bnc  = fromIntegral . blackKnightCount
        wpc  = fromIntegral . whitePawnCount
        bpc  = fromIntegral . blackPawnCount
        wdpc = fromIntegral . whiteDoubledPawnCount
        bdpc = fromIntegral . blackDoubledPawnCount
        wipc = fromIntegral . whiteIsolatedPawnCount
        bipc = fromIntegral . blackIsolatedPawnCount
        wbpc = fromIntegral . whiteBlockedPawnCount
        bbpc = fromIntegral . blackBlockedPawnCount
        wlmc = fromIntegral . whiteLegalMoveCount
        blmc = fromIntegral . blackLegalMoveCount

--------------------
-- PIECE COUNTING --
--------------------

-- ___ :: Board -> Int
whitePawnCount   = popCount . whitePawn
whiteKnightCount = popCount . whiteKnight
whiteBishopCount = popCount . whiteBishop
whiteRookCount   = popCount . whiteRook
whiteQueenCount  = popCount . whiteQueen
whiteKingCount   = popCount . whiteKing

blackPawnCount   = popCount . blackPawn
blackKnightCount = popCount . blackKnight
blackBishopCount = popCount . blackBishop
blackRookCount   = popCount . blackRook
blackQueenCount  = popCount . blackQueen
blackKingCount   = popCount . blackKing

-------------------
-- DOUBLED PAWNS --
-------------------

-- | Tags all white doubled pawns
whiteDoubledPawns :: Board -> Word64
whiteDoubledPawns = (.|.) <$> whiteNorthDoubledPawns <*> whiteSouthDoubledPawns
  where whiteNorthDoubledPawns = (.&.) <$> whitePawn <*> Pawn.northSpan . whitePawn
        whiteSouthDoubledPawns = (.&.) <$> whitePawn <*> Pawn.southSpan . whitePawn

-- | Counts all white doubled pawns
whiteDoubledPawnCount :: Board -> Int
whiteDoubledPawnCount = popCount . whiteDoubledPawns

-- | Tags all black doubled pawns
blackDoubledPawns :: Board -> Word64
blackDoubledPawns = (.|.) <$> blackNorthDoubledPawns <*> blackSouthDoubledPawns
  where blackNorthDoubledPawns = (.&.) <$> blackPawn <*> Pawn.northSpan . blackPawn
        blackSouthDoubledPawns = (.&.) <$> blackPawn <*> Pawn.southSpan . blackPawn

-- | Counts all black doubled pawns
blackDoubledPawnCount :: Board -> Int
blackDoubledPawnCount = popCount . blackDoubledPawns

--------------------
-- ISOLATED PAWNS --
--------------------

-- | Tags all white isolated pawns
whiteIsolatedPawns :: Board -> Word64
whiteIsolatedPawns = (.&.) <$> whitePawn <*> complement . Pawn.fullAttackFileFill . whitePawn

-- | Counts all white isolated pawns
whiteIsolatedPawnCount :: Board -> Int
whiteIsolatedPawnCount = popCount . whiteIsolatedPawns

-- | Tags all black isolated pawns
blackIsolatedPawns :: Board -> Word64
blackIsolatedPawns = (.&.) <$> blackPawn <*> complement . Pawn.fullAttackFileFill . blackPawn

-- | Counts all black isolated pawns
blackIsolatedPawnCount :: Board -> Int
blackIsolatedPawnCount = popCount . blackIsolatedPawns

-------------------
-- BLOCKED PAWNS --
-------------------

-- | Tags all white blocked pawns
whiteBlockedPawns :: Board -> Word64
whiteBlockedPawns = (.&.) <$> north . whitePawn <*> piece

-- | Counts all white blocked pawns
whiteBlockedPawnCount :: Board -> Int
whiteBlockedPawnCount = popCount . whiteBlockedPawns

-- | Tags all black blocked pawns
blackBlockedPawns :: Board -> Word64
blackBlockedPawns = (.&.) <$> south . blackPawn <*> piece

-- | Counts all black blocked pawns
blackBlockedPawnCount :: Board -> Int
blackBlockedPawnCount = popCount . blackBlockedPawns

-----------------
-- LEGAL MOVES --
-----------------

-- Now the real fun begins

whiteLegalMoveCount = undefined
blackLegalMoveCount = undefined

