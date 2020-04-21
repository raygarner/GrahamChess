module Init where

import TypeDefs

addPawns :: AllPieces
addPawns = [ (Pawn, c, (m, n), 0) | n <- [0..7], (c, m) <- [(White, 6), (Black, 1)] ]

addRooks :: AllPieces
addRooks = [ (Rook, c, (m, n), 0) | n <- [0,7], (c, m) <- [(White, 7), (Black, 0)] ]

addKnights :: AllPieces
addKnights = [ (Knight, c, (m, n), 0) | n <- [1,6], (c, m) <- [(White, 7), (Black, 0)] ]

addBishops :: AllPieces
addBishops = [ (Bishop, c, (m, n), 0) | n <- [2,5], (c, m) <- [(White, 7), (Black, 0)] ]

addQueens :: AllPieces
addQueens = [ (Queen, c, (m, 3), 0) | (c, m) <- [(White, 7), (Black, 0)] ]

addKings :: AllPieces
addKings = [ (King, c, (m, 4), 0) | (c, m) <- [(White, 7), (Black, 0)] ]

addAllPieces :: AllPieces
addAllPieces = addPawns ++ addRooks ++ addKnights ++ addBishops ++ addQueens ++ addKings
