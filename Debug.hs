module Debug where

import TypeDefs


addTestKings :: AllPieces
addTestKings = [ (King, c, (m,3), 0) | (c, m) <- [(White, 5), (Black, 7)] ]

addTestPawns :: AllPieces
addTestPawns = [ (Pawn, Black, p, 0) | p <- [(4,1), (2,3), (3,4), (1,6), (6,5)] ]

addTestKnights :: AllPieces
addTestKnights = [ (Knight, c, (m, 1), 0) | (c,m) <- [(White,3), (Black,1)] ]

addTestQueens :: AllPieces
addTestQueens = [(Queen, White, (2,7), 0)]

addTestRooks :: AllPieces
addTestRooks = [(Rook, White, (5,6), 0)]

addTestBishops :: AllPieces
addTestBishops = [(Bishop, c, (m,n), 0) | (c, m, n) <- [(Black,6,1 ), (White,0,5)] ]

addTestPieces :: AllPieces
addTestPieces = addTestKings ++ addTestPawns ++ addTestKnights ++ addTestQueens ++ addTestRooks ++ addTestBishops
