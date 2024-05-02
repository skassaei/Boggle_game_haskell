module Boggle (boggle) where

{--
    The boggle function with helper functions.
    boggle accepts two arguments, a N*N grid and a list of legal words.
    The function returns a list of tuples of the word found in the grid.
    [ (“seat”, [ (1,0), (0,0), (0,1), (1,1) ]) ]
    Test the code by running 'cabal test' from the tester_hs_simple directory.
--}
    

  boggle board listwordsLegal = do 
    let boardLength = length board* length board
    let result = searchWord boardLength board listwordsLegal []
    result

  searchWord boardLength board [] finalResult = finalResult
  searchWord boardLength board (word:xs) finalResult = do
    let start_points = search_row (head word) board [] 0
    let wordPossiblePath = search_from start_points (tail word) board [] word False
    if not (null wordPossiblePath) then searchWord boardLength board xs (finalResult++ [(word, wordPossiblePath)])
    else searchWord boardLength board xs finalResult


  search_from [] [] board found_path completeWord False= [] -- Last letter. Not found
  search_from poss_points [] board found_path completeWord False= found_path++[head poss_points] 
  search_from [] word board found_path completeWord False= [] -- ran out of next points while we still have the word: so it hasn't been found
  search_from poss_points word board found_path completeWord True= [] -- have been found in another function

-- searchs for the next possition and iteratively and stops when whole word is found
  search_from (position:xs) word board found_path completeWord stop= do 
    let directions = getAdjacentPositions position board 
    let validNexts = getValidPositions (head word) board directions (found_path++[position]) []
    let possible_path = search_from validNexts (tail word) board (found_path++[position]) completeWord stop
    let newStop = (length completeWord == length possible_path)
    possible_path++search_from xs word board (found_path) completeWord newStop
    

--A valid point is unique, related to the previous point and equal to the letter we are looking for
-- Returns all the possible valid points or empty
  getValidPositions letter board [] possible_path validDire = validDire
  getValidPositions letter board (position:rest) possible_path validDire = do 
    let (x,y) = position
    if letter == (board!!x)!!y && not (elem position possible_path) then getValidPositions letter board rest possible_path (validDire++[position])
    else getValidPositions letter board rest possible_path validDire


-- Get all valid adjacent positions from a given coordinate
  getAdjacentPositions (x, y) board = filter isValidPosition 
    [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]
    where
      isValidPosition (x, y) = x >= 0 && x < length board && y >= 0 && y < length board

  search_row letter [] found_Positions row = found_Positions --base case: after went through the whole board
  search_row letter (head:xs) found_Positions row = do 
     let found = search_col letter head [] row 0
     search_row letter xs (found_Positions++found) (row+1)

--search_row lookingforLetter  col_list found_positions Num_row Num_col
  search_col letter [] found row col= found
  search_col letter (item:xs) found row col = 
    if (letter==item) then search_col letter xs (found++[(row,col)]) row (col+1)
    else search_col letter xs found row (col+1)


