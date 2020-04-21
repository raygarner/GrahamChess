module TypeDefs where

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
                 deriving (Read, Eq, Ord, Show)

--type Col = Bool
data Colour = Black | White
              deriving (Read, Eq, Show)

type Pos = (Int, Int) -- row, column

type movecount = Int

type Piece = (PieceType, Colour, Pos, movecount) -- type, colour of piece on square, position, ammount of moves it has made

type AllPieces = [Piece]

--type Move = (Piece, Pos, Bool) -- piece, target square, validity

type Move = (Int, Int) -- row difference, column difference
