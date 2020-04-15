module TypeDefs where

type Col = Bool

type Pos = (Int, Int) -- row, column

type Piece = (Char, Col, Pos) -- type, colour of piece on square, position

type AllPieces = [Piece]

type Move = (Piece, Pos, Bool) -- piece, target square, validity

