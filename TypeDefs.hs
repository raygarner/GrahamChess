module TypeDefs where

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
                 deriving (Read, Eq, Ord, Show)

--type Col = Bool
data Colour = Black | White
              deriving (Read, Eq, Show)

data GamePoint = Opening | Middle | End
                 deriving (Eq, Ord, Show, Read)

type Pos = (Int, Int) -- row, column

type Movecount = Int

type Piece = (PieceType, Colour, Pos, Movecount) -- type, colour of piece on square, position, ammount of moves it has made

type AllPieces = [Piece]

--type Move = (Piece, Pos, Bool) -- piece, target square, validity

type Move = (Int, Int) -- row difference, column difference

type Node = (AllPieces, AllPieces, Float, Float) --prev board, current board, white eval for the board, black eval for the board

type Tree = [Node]
