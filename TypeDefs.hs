module TypeDefs where

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
                 deriving (Eq, Ord)

type Col = Bool

type Pos = (Int, Int) -- row, column

type Piece = (PieceType, Col, Pos) -- type, colour of piece on square, position

type AllPieces = [Piece]

--type Move = (Piece, Pos, Bool) -- piece, target square, validity

type Move = (Int, Int) -- row difference, column difference



