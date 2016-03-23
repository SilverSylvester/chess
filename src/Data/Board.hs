--------------------------------------------------------------------
-- |
-- Module     : Data.Board
-- Copyright  : (c) Conor Reynolds 2016
-- License    : MIT
-- Maintainer : reynolds.conor@gmail.com
-- Stability  : experimental
-- Portability: non-portable
--              Module responsible for defining the chess board and
--              the most common operations associated with it.
--
--------------------------------------------------------------------

module Data.Board (
                  -- Data types
                  Board(..)

                  -- Atomic combinators
                  , piece
                  , empty
                  , white
                  , black
                  , pawn
                  , knight
                  , bishop
                  , rook
                  , queen
                  , king 
                    
                  -- Composite combinators
                  , whitePiece
                  , blackPiece

                  , whitePawn
                  , whiteKnight
                  , whiteBishop
                  , whiteRook
                  , whiteQueen
                  , whiteKing

                  , blackPawn
                  , blackKnight
                  , blackBishop
                  , blackRook
                  , blackQueen
                  , blackKing

                  , hammingDistance
                  , verticalFlip
                  , horizontalFlip

                  , isFull
                  , isEmpty
                  
                  -- Mainly to prevent wrapping when inconvenient
                  , notFileA
                  , notFileH

                  -- For the purposes of debugging (remove later)
                  , showBoard
                  , startingBoard
                  ) where

import Control.Applicative ((<$>), (<*>))

import Data.Bits
import Data.Digits
import Data.List
import Data.Word

---------------------------------
-- Board Data type + Instances --
---------------------------------

-- | See Bitboards:
--   https://chessprogramming.wikispaces.com/Bitboards
--   
--   _111, as a 'position', is not occupied.
data Board = Board
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64

instance Show Board where
    show = printBoard

-------------------------
-- Bitwise combinators --
-------------------------

-- ___ :: Board -> Word64
piece  (Board a b c d) = b .|. c .|. d
empty  (Board a b c d) = complement (b .|. c .|. d)

white  (Board a _ _ _) = a
black  (Board a _ _ _) = complement a

pawn   (Board _ b c d) = b .&. complement c .&. d
knight (Board _ b c d) = b .&. complement (c .|. d)
bishop (Board _ b c d) = complement (b .|. c) .&. d
rook   (Board _ b c d) = complement b .&. c .&. complement d
queen  (Board _ b c d) = complement b .&. c .&. d
king   (Board _ b c d) = b .&. c .&. complement d

whitePiece  = (.&.) <$> white <*> piece
blackPiece  = (.&.) <$> black <*> piece

whitePawn   = (.&.) <$> white <*> pawn
whiteKnight = (.&.) <$> white <*> knight
whiteBishop = (.&.) <$> white <*> bishop
whiteRook   = (.&.) <$> white <*> rook
whiteQueen  = (.&.) <$> white <*> queen
whiteKing   = (.&.) <$> white <*> king

blackPawn   = (.&.) <$> black <*> pawn
blackKnight = (.&.) <$> black <*> knight
blackBishop = (.&.) <$> black <*> bishop
blackRook   = (.&.) <$> black <*> rook
blackQueen  = (.&.) <$> black <*> queen
blackKing   = (.&.) <$> black <*> king

-- | Defines a metric for how different any two positions are
hammingDistance :: Word64 -> Word64 -> Int
hammingDistance a b = popCount (a `xor` b)

-- | Vertical flip about the 'central' rank
verticalFlip :: Word64 -> Word64
verticalFlip b =
        shiftL b 56
    .|. shiftL b 40 .&. 0x00ff000000000000
    .|. shiftL b 24 .&. 0x0000ff0000000000
    .|. shiftL b 8  .&. 0x000000ff00000000
    .|. shiftR b 8  .&. 0x00000000ff000000
    .|. shiftR b 24 .&. 0x0000000000ff0000
    .|. shiftR b 40 .&. 0x000000000000ff00
    .|. shiftR b 56

-- | Horizontal flip about the 'central' file
horizontalFlip :: Word64 -> Word64
horizontalFlip b =
    let k1 = 0x5555555555555555
        k2 = 0x3333333333333333
        k4 = 0x0f0f0f0f0f0f0f0f
        x  = shiftR b 1 .&. k1 .|. shiftL (b .&. k1) 1
        y  = shiftR x 2 .&. k2 .|. shiftL (x .&. k2) 2
    in       shiftR y 4 .&. k4 .|. shiftL (y .&. k4) 4

--------------------
-- BOOLEAN CHECKS --
--------------------

-- | Checks if a particular board tag is empty
--   (i.e. no square is tagged)
isEmpty :: Word64 -> Bool
isEmpty = (== 0)

-- | Checks if a particular board tab is full
--   (i.e. all squares are tagged)
isFull :: Word64 -> Bool
isFull = (== complement 0)

----------
-- MISC --
----------

-- | Bitmasks for edge files. Prevents wrapping.
--   No real need for other file combinators (yet).
notFileA, notFileH :: Word64
notFileA = 0xfefefefefefefefe
notFileH = 0x7f7f7f7f7f7f7f7f

-- | Standard starting positions for all pieces.
startingBoard = (\(a:b:c:d:_) -> Board a b c d) $ map (unDigits 2) $ transpose
    [ [0,0,1,0], [0,1,0,0], [0,0,0,1], [0,1,1,0], [0,0,1,1], [0,0,0,1], [0,1,0,0], [0,0,1,0]
    , [0,1,0,1], [0,1,0,1], [0,1,0,1], [0,1,0,1], [0,1,0,1], [0,1,0,1], [0,1,0,1], [0,1,0,1]
    , [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0]
    , [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0]
    , [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0]
    , [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0]
    , [1,1,0,1], [1,1,0,1], [1,1,0,1], [1,1,0,1], [1,1,0,1], [1,1,0,1], [1,1,0,1], [1,1,0,1]
    , [1,0,1,0], [1,1,0,0], [1,0,0,1], [1,1,1,0], [1,0,1,1], [1,0,0,1], [1,1,0,0], [1,0,1,0]
    ]

-- | For debugging.
showBoard :: Word64 -> IO ()
showBoard = putStr . unlines . map (unwords . reverse . map show) . group8 . pad . digits 2

-- | Handy in many cases, but probably belongs in a Utils library.
group8 :: [a] -> [[a]]
group8 [] = []
group8 xs = take 8 xs : group8 (drop 8 xs)

-- | Pads a list of 64 bit integers with zeros until the list is
--   64 in length (does *not* truncate if the list is too long).
pad :: [Word64] -> [Word64]
pad ds = replicate (64 - length ds) 0 ++ ds

-- | The representation isn't perfect, but is more than enough for
--   debugging. Maybe try to show the coordinates on the left and
--   bottom.
printBoard :: Board -> String
printBoard = ("+----+----+----+----+----+----+----+----+\n"++)
           . intercalate "\n"
           . map ((++ "|\n+----+----+----+----+----+----+----+----+")
                    . concatMap match . reverse)
           . group8
           . transpose
           . map (pad . digits 2)
           . (\(Board a b c d) -> [a,b,c,d])
    -- Note that the colours here, in a Unicode sense, are 'wrong'. White looks
    -- like black and black looks like white, so I've just switched them. The
    -- actual colour of each piece is indicated on the right.
    where match [_,0,0,0] = "|    "
          match [0,1,0,1] = "| ♙  " -- White pawn
          match [1,1,0,1] = "| ♟  " -- Black pawn
          match [0,1,0,0] = "| ♘  " -- White knight
          match [1,1,0,0] = "| ♞  " -- Black knight
          match [0,0,0,1] = "| ♗  " -- White bishop
          match [1,0,0,1] = "| ♝  " -- Black bishop
          match [0,0,1,0] = "| ♖  " -- White rook
          match [1,0,1,0] = "| ♜  " -- Black rook
          match [0,0,1,1] = "| ♕  " -- White queen
          match [1,0,1,1] = "| ♛  " -- Black queen
          match [0,1,1,0] = "| ♔  " -- White king
          match [1,1,1,0] = "| ♚  " -- Black king
          match x         = show x  -- Something went wrong

