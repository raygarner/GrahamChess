import Util
import TypeDefs
import Init
import Debug

-- some crude evaluations

evalPiece :: Piece -> AllPieces -> Float
evalPiece a ps = fromIntegral (length (legalMoves a ps)) * (pieceVal a)

pieceVal :: Piece -> Float
pieceVal (Pawn,_,_) = 1.0
pieceVal (Knight,_,_) = 3.0
pieceVal (Bishop,_,_) = 3.5
pieceVal (Rook,_,_) = 5
pieceVal (Queen,_,_) = 9

totalVal :: Colour -> AllPieces -> Float
totalVal a ps = sum [ evalPiece x ps | x <- ps, getColour x == a ]
