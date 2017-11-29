import Game

-- newline x xRange takes an integer x and an array of integers xRange and returns a newline if x is the max
-- value in xRange, if not, it will return an empty array.
newline :: Int -> [Int] -> [Char]
newline x xRange
  | x == maximum xRange = ['\n']
  | otherwise = []

-- plot x y board deadBoard takes two integers x, y and a two boards board, deadBoard and
-- will print a '■' if the point (x, y) is in board and will print a '-' if point (x,y) is in deadBoard.
plot :: Int -> Int -> Board -> Board -> Char
plot x y board deadBoard
  | (x, y) `elem` board = '■'
  | (x, y) `elem` deadBoard = '□'
  | otherwise = ' '

-- deadCells board takes in a board and will return a board with only the dead cells in it.
deadCells :: Board -> Board
deadCells board = [(a,b) | (a,b) <- newBoard, (a,b) `notElem` board]
  where
    newBoard = [(x,y) | x <- [0..width - 1], y <- [0..height - 1]]

-- buildBoard takes two boards board, deadBoard and will plot the corresponding board based on these boards.
buildBoard :: Board -> Board -> String
buildBoard board deadBoard = do
  y <- [0..width - 1]
  x <- [0..width - 1]
  [plot x y board deadBoard] ++ newline x ([0..width - 1])

-- printBoard takes in a board and will output the corresponding board to the console.
printBoard :: Board -> IO ()
printBoard board = do
  putStrLn (buildBoard board (deadCells board))
  putStrLn ""

-- main function to start the program.
main = do
  putStr ("Input number of generations: ")
  line <- getLine
  let numGens = (read line :: Int)
  putStr ("\nInput the initial alive cells: ")
  line2 <- getLine
  let input = (read line2 :: [(Int, Int)])
  mapM_ printBoard . take numGens $ iterate step input
