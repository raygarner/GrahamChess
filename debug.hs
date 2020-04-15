import TypeDefs

addPawns :: AllPieces
addPawns = [ (Pawn, j, (k, y)) | y <- [0..7], (j, k) <- [(White, 6), (Black, 1)] ]

addRooks :: AllPieces
addRooks = [ (Rook, j, (k, y)) | y <- [0,7], (j, k) <- [(White, 7), (Black, 0)] ]

addKnights :: AllPieces
addKnights = [ (Knight, j, (k, y)) | y <- [1,6], (j, k) <- [(White, 7), (Black, 0)] ]

addBishops :: AllPieces
addBishops = [ (Bishop, j, (k, y)) | y <- [2,5], (j, k) <- [(White, 7), (Black, 0)] ]

addQueens :: AllPieces
addQueens = [ (Queen, j, (k, 3)) | (j, k) <- [(White, 7), (Black, 0)] ]

addKings :: AllPieces
addKings = [ (King, j, (k, 4)) | (j, k) <- [(White, 7), (Black, 0)] ]

addAllPieces :: AllPieces
addAllPieces = addPawns ++ addRooks ++ addKnights ++ addBishops ++ addQueens ++ addKings
