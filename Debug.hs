module Debug where

import TypeDefs
import Util
import Init
import UI

addTestKings :: AllPieces
addTestKings = [ (King, c, (m,3), 5) | (c, m) <- [(White, 5), (Black, 7)] ]

addTestPawns :: AllPieces
addTestPawns = [ (Pawn, Black, p, 5) | p <- [(4,1), (2,3), (3,4), (1,6), (6,5)] ]

addTestKnights :: AllPieces
addTestKnights = [ (Knight, c, (m, 1), 5) | (c,m) <- [(White,3), (Black,1)] ]

addTestQueens :: AllPieces
addTestQueens = [(Queen, White, (2,7), 5)]

addTestRooks :: AllPieces
addTestRooks = [(Rook, White, (5,6), 5)]

addTestBishops :: AllPieces
addTestBishops = [(Bishop, c, (m,n), 5) | (c, m, n) <- [(Black,6,1 ), (White,0,5)] ]

addTestPieces :: AllPieces
addTestPieces = addTestKings ++ addTestPawns ++ addTestKnights ++ addTestQueens ++ addTestRooks ++ addTestBishops

buggedPosition :: AllPieces
buggedPosition = executeMove (Queen, White, (7,3), 0) (-4,4) (executeMove (Pawn, Black, (1,4), 0) (2,0) (executeMove (Pawn, White, (6,4), 0) (-2,0) addAllPieces))


-- pieces for endgameboard.png

addEnd1Kings :: AllPieces
addEnd1Kings = [ (King, c, (m,n), 3) | (c,m,n) <- [(White,4,4), (Black,2,5)]]

addBlackEnd1Pawns :: AllPieces
addBlackEnd1Pawns = [ (Pawn, Black, (m,n), 4) | (m,n) <- [(2,0),(2,1),(2,7),(3,6),(4,6)]]

addWhiteEnd1Pawns :: AllPieces
addWhiteEnd1Pawns = [ (Pawn,White,(m,n),4) | (m,n) <- [(6,0),(5,1),(4,3),(6,7)]]

addEnd1Bishops :: AllPieces
addEnd1Bishops = [(Bishop, c, (m,n),4) | (c,m,n) <- [(White,0,1), (Black,1,4)]]

addEnd1Pieces :: AllPieces
addEnd1Pieces = addEnd1Kings ++ addBlackEnd1Pawns ++ addWhiteEnd1Pawns ++ addEnd1Bishops


-- pieces for endgameboard2.png

addEnd2Kings :: AllPieces
addEnd2Kings = [ (King,c,(m,n),3) | (c,m,n) <- [(White,0,0), (Black,7,5)]]

addEnd2Pawns :: AllPieces
addEnd2Pawns = [ (Pawn,White,(m,n),3) | (m,n) <- [(1,5),(1,7),(3,6)]]

addEnd2Rooks :: AllPieces
addEnd2Rooks = [ (Rook, White, (m,n),3) | (m,n) <- [(5,7),(6,1)]]

addEnd2Pieces :: AllPieces
addEnd2Pieces = addEnd2Kings ++ addEnd2Pawns ++ addEnd2Rooks
