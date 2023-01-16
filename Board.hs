module Board where
	mkBoard = [[0,0,0,0,0,0,0],[0,0,0,0,0,0,0],[0,0,0,0,0,0,0],[0,0,0,0,0,0,0],[0,0,0,0,0,0,0],[0,0,0,0,0,0,0]]
	mkPlayer = 1
	mkOpponent = 2

-- Picks column of board
	pickCol :: [[a]] -> Int -> [a]
	pickCol bd i = map(!! i) bd

-- Checks if the column chosen has space available
	isSlotOpen :: [Integer] -> Bool
	isSlotOpen (x:y) 
		| x == 0 = True
		| otherwise = False

-- Checks if the board is full
	isFull :: [Integer] -> Bool
	isFull [] = True
	isFull (x:y)
		| x == 0 = False
		| otherwise = isFull y

-- Drops the piece in the chosen slot
	drops :: [[Integer]] -> Int -> Integer -> [[Integer]]
	drops (x:y) i p
		| length (x:y) == 1 = [take i x ++ [p] ++ drop (i+1) x]
		| head y !! i /= 0 = (take i x ++ [p] ++ drop (i+1) x): y
		| otherwise = x: drops y i p

-- Goes through the board for the UI
	traverseBoard :: [[Integer]] -> [Char]
	traverseBoard [] = ""
	traverseBoard (x:y)
		| length x == 7 = boardToString x ++ traverseBoard y

-- Gets row of board and prints it out
	boardToString :: [Integer] -> String
	boardToString [] = "\n"
	boardToString (x:y)
		| x == 0 = ". " ++ boardToString y
		| x == 1 = "X " ++ boardToString y
		| x == 2 = "O " ++ boardToString y

-- Changes player's turn
	change :: Integer -> Integer
	change n
		| n == 1 = mkOpponent
		| otherwise = mkPlayer

-- Checks for all possible wins
	win :: [[Integer]] -> Integer -> Bool
	win bd p = if ((traverseCol bd 0 p) == True) || ((traverseRow bd p) == True) || ((forwardDiagonals bd p 5 0) == True) || ((forwardDiagonals (map reverse bd) p 5 0) == True) then True else False

-- To check if there is win in column or row
	columnRow :: [Integer] -> Integer -> Integer -> Bool
	columnRow _ _ 4 = True
	columnRow [] _ _ = False
	columnRow (x:y) p n
		| x == p = columnRow y p (n+1)
		| otherwise = columnRow y p 0
		
-- Gets one row at a time
	traverseRow :: [[Integer]] -> Integer -> Bool
	traverseRow bd p = if null bd then False else if (columnRow (head bd) p 0) == True then True else traverseRow (tail bd) p
		
-- Gets one column at a time
	traverseCol :: [[Integer]] -> Int -> Integer -> Bool
	traverseCol bd c p = if c == 7 then False else if (columnRow (map(!! c) bd) p 0) == True then True else traverseCol bd (c+1) p
	
-- Checks for wins for all possible forward diagonals
	forwardDiagonals :: [[Integer]] -> Integer -> Int -> Int -> Bool
	forwardDiagonals [] p row col = False;
	forwardDiagonals bd p row col
		| (row>3 && col<4) = checkWinRow (getDiagonal bd row col 0) p 0 || forwardDiagonals bd p (row-1) col || forwardDiagonals bd p row (col+1)
		| otherwise = False

-- Gets a list for every diagonal	
	getDiagonal :: [[Integer]] -> Int -> Int -> Integer -> [Integer]
	getDiagonal [] _ _ _ = []
	getDiagonal bd row col 3 = [fromIntegral (getElem bd row col)]
	getDiagonal bd row col i = [fromIntegral (getElem bd row col)] ++ getDiagonal bd (row-1) (col+1) (i+1)

-- Gets the element to fill up the diagonal list
	getElem :: [[a]] -> Int -> Int -> a
	getElem bd row col = (bd!!row) !! col

-- Checks to see if the diagonal was a winner
	checkWinRow :: [Integer] -> Integer -> Integer -> Bool
	checkWinRow [] p c = False
	checkWinRow (x:y) p c
		| (c == 4 || (c == 3 && x==p)) = True
		| x == p = checkWinRow y p (c+1)
		| otherwise = checkWinRow y p 0