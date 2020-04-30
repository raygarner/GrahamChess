module Debug where

import TypeDefs
import Util
import Init

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
