type Point = (Int, Int)
type Grid = [Point]
type Board = [Piece]
type Tile  = (Piece, Point)
data Piece = D | W | B deriving (Eq, Show)
type State = [Tile]
type Slide = (Point,Point)
type Jump = (Point,Point,Point)
type Move = (Point,Point)
data Next a = Next {usedDepth :: Int, newBoard :: a, seenBoards :: [a], cplayer :: Piece}
data Tree a = Node {depth :: Int, board :: a, nextBoards :: [Tree a]} deriving (Show)
type BoardTree = Tree Board

grid3::[Point]
grid3 = [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(3,1),(0,2),(1,2),(2,2),(3,2),(4,2),(0,3),(1,3),(2,3),(3,3),(0,4),(1,4),(2,4)]

grid2:: [Point]
grid2 = [(0,0),(1,0),(0,1),(1,1),(2,1),(0,2),(1,2)]

state2 :: State
state2 = [(B,(0,0)),(D, (1,0)), (D, (0,1)),(B,(1,1)),(D,(2,1)),(W,(0,2)),(D,(1,2))]

state2_from :: State
state2_from = [(B,(0,0)),(B,(1,0)),(D,(0,1)),(B,(1,1)),(D,(2,1)),(W,(0,2)),(D,(1,2))]

board2 :: Board
board2 = [B,D,D,B,D,W,D]


history2 = []


moves2 :: [Move]
moves2 = [((0,0),(1,0)),((0,0),(0,1)),((0,0),(1,2)),((1,1),(2,1)),((1,1),(1,2))]

slides:: [Slide]
slides = [((1,4),(1,5)),((1,4),(2,4)),((1,4),(0,5)),((1,4),(2,3)),((1,4),(1,3)),((1,4),(0
 ,4))]

slides2 :: [Slide]
slides2 = [((0,0),(1,0)),((0,0),(1,1)),((0,0),(0,1)),((1,0),(0,0)),((1,0),(2,1)),((1,0),
 (1,1)),((0,1),(0,0)),((0,1),(1,1)),((0,1),(0,2)),((1,1),(0,1)),((1,1),(0,0)),((1,1),
 (1,0)),((1,1),(2,1)),((1,1),(1,2)),((1,1),(0,2)),((2,1),(1,1)),((2,1),(1,0)),
 ((2,1),(1,2)),((0,2),(0,1)),((0,2),(1,1)),((0,2),(1,2)),((1,2),(0,2)),((1,2),(1,1)),
 ((1,2),(2,1))]

jumps2 :: [Jump]
jumps2 = [((0,0),(1,1),(1,2)),((1,0),(1,1),(0,2)),((0,1),(1,1),(2,1)),((2,1),(1,1),(0,1)),
 ((0,2),(1,1),(1,0)),((1,2),(1,1),(0,0))]

generateSlides :: Grid -> Int -> [Slide] -- To Be Completed 
generateSlides [] n = []
generateSlides b n = generateSlides_helper b b n

generateSlides_helper b points_left n
 | points_left == [] = []
 | otherwise = 
    (generateValidSlidesForOnePoint b (generateAllSlidesForOnePoint (head points_left) n)) ++
     (generateSlides_helper b (tail points_left) n)

-- given a grid and a list of slides of a point, filters out all the invalid slides
-- based on the grid, and returns that list of valid slides in the end.
generateValidSlidesForOnePoint:: Grid -> [Slide] -> [Slide]
generateValidSlidesForOnePoint b slides
 | b == [] = []
 | slides == [] = []
 | elem (snd (head slides)) b = (head slides):(generateValidSlidesForOnePoint b (tail slides))
 | otherwise = (generateValidSlidesForOnePoint b (tail slides))

-- given a point, returns all the slides for that point in the order of:
-- starting from the left point and go clockwise.
generateAllSlidesForOnePoint :: Point -> Int -> [Slide]
generateAllSlidesForOnePoint p int 
 | (snd p) < (div (2*n-2) 2) = 
      [(p, ((fst p)-1, snd p)),
       (p, (fst lp, (snd lp)-1)),
       (p, (fst p, (snd p)-1)),
       (p, ((fst p)+1, snd p)),
       (p, (fst rp, (snd rp)+1)),
       (p, (fst p, (snd p)+1))]
 | (snd p) > (div (2*n-2) 2) = 
      [(p, ((fst p)-1, snd p)),
       (p, (fst p, (snd p)-1)),
       (p, (fst rp, (snd rp)-1)),
       (p, ((fst p)+1, snd p)),
       (p, (fst p, (snd p)+1)),
       (p, (fst lp, (snd lp)+1))]
 | otherwise = 
      [(p, ((fst p)-1, snd p)),
       (p, (fst lp, (snd lp)-1)), 
       (p, (fst p, (snd p)-1)),
       (p, ((fst p)+1, snd p)),
       (p, (fst p, (snd p)+1)),
       (p, (fst lp, (snd rp)+1))] 
 where
 n = fromIntegral int
 lp = ((fst p)-1, snd p)
 rp = ((fst p)+1, snd p)


--removes x from x_list
--delete x x_list = [result|result<-x_list, result/=x]

-- get the first, second, or third element from a jump
get_jump_thd:: Jump -> Point
get_jump_thd (a,b,c) = c

get_jump_fst:: Jump -> Point
get_jump_fst (a,b,c) = a

get_jump_snd:: Jump -> Point
get_jump_snd (a,b,c) = b

{--old generateSlides
-- given a point, returns all the slides for that point
generateAllSlidesForOnePoint :: Point -> Int -> [Slide]
generateAllSlidesForOnePoint p int 
 | (snd p) < (div (2*n-2) 2) = 
      list ++ [(p, (fst lp, (snd lp)-1)), (p, (fst rp, (snd rp)+1))]
 | (snd p) > (div (2*n-2) 2) = 
      list ++ [(p, (fst lp, (snd lp)+1)), (p, (fst rp, (snd rp)-1))]
 | otherwise = 
      list ++ [(p, (fst lp, (snd lp)-1)), (p, (fst lp, (snd rp)+1))]
 where
 n = fromIntegral int
 list = [(p, (fst p, (snd p)-1)),
          (p, (fst p, (snd p)+1)),
          (p, ((fst p)-1, snd p)),
          (p, ((fst p)+1, snd p))]
 lp = ((fst p)-1, snd p)
 rp = ((fst p)+1, snd p)
--}


generateLeaps :: Grid -> Int -> [Jump] -- To Be Completed 
generateLeaps [] n = []
generateLeaps b n = generateLeaps_helper b b n

generateLeaps_helper b points_left n
 | points_left == [] = []
 | otherwise = 
    (generateValidLeapsForOnePoint b (generateAllLeapsForOnePoint (head points_left) n)) ++
     (generateLeaps_helper b (tail points_left) n)

-- given a grid and a list of leaps of a point, filters out all the invalid leaps
-- based on the grid, and returns that list of valid leaps in the end.
generateValidLeapsForOnePoint:: Grid -> [Jump] -> [Jump]
generateValidLeapsForOnePoint b leaps
 | b == [] = []
 | leaps == [] = []
 | elem (get_jump_thd (head leaps)) b = (head leaps):(generateValidLeapsForOnePoint b (tail leaps))
 | otherwise = (generateValidLeapsForOnePoint b (tail leaps))

-- given a point, returns all the leaps for that point in the order of:
-- starting from the left point and go clockwise.
generateAllLeapsForOnePoint :: Point -> Int -> [Jump]
generateAllLeapsForOnePoint p n =
 [(p, (snd (one s)), ((fst (snd (one s)))-1, snd (snd (one s)))),
  (p, (snd (two s)), (fortwo (snd (two s)) n)),
  (p, (snd (three s)), (forthree (snd (three s)) n)),
  (p, (snd (four s)), ((fst (snd (four s)))+1, snd (snd (four s)))),
  (p, (snd (five s)), (forfive (snd (five s)) n)),
  (p, (snd (six s)), (forsix (snd (six s)) n))]
 where
 s = generateAllSlidesForOnePoint p n


fortwo p n 
 | (snd p) <= (div (2*n-2) 2) = ((fst p)-1, (snd p)-1)
 | otherwise = (fst p, (snd p)-1)

forthree p n
 | (snd p) <= (div (2*n-2) 2) = (fst p, (snd p)-1)
 | otherwise = ((fst p)+1, (fst p)-1)

forfive p n
 | (snd p) >= (div (2*n-2) 2) = (fst p, (snd p)+1)
 | otherwise = ((fst p)+1, (snd p)+1)

forsix p n
 | (snd p) >= (div (2*n-2) 2) = ((fst p)-1, (snd p)+1)
 | otherwise = (fst p, (snd p)+1)

one list = (head list)
two list = (head (tail list))
three list = (head (tail (tail list)))
four list = (head (tail(tail(tail list))))
five list = (head (tail(tail(tail (tail list)))))
six list = (head (tail(tail(tail (tail (tail list))))))


moveGenerator :: State -> [Slide] -> [Jump] -> Piece -> [Move]
moveGenerator state slides jumps player
 | state == [] = []
 | (fst cur_tile) == player =
    (vSforOnePiece slides (snd cur_tile) state) ++
    (vJforOnePiece jumps (snd cur_tile) state player) ++
    (moveGenerator (tail state) slides jumps player)
 | otherwise = moveGenerator (tail state) slides jumps player
 where
  cur_tile = (head state)

-- produces all the valid slides for a piece on the board given the state
-- of the board.
vSforOnePiece :: [Slide] -> Point -> State -> [Move]
vSforOnePiece slides point state
 | slides == [] = []
 | ((fst (head slides)) == point) && (check_moveTo_empty (snd (head slides)) state) =
    (head slides):(vSforOnePiece (tail slides) point state)
 | otherwise = vSforOnePiece (tail slides) point state

-- produces all the valid jumps for a piece on the board given the state
-- of the board.
vJforOnePiece :: [Jump] -> Point -> State -> Piece -> [Move]
vJforOnePiece jumps point state player
 | jumps == [] = []
 | ((get_jump_fst (head jumps)) == point) && 
   (check_moveTo_EmptyorOppPiece (get_jump_thd (head jumps)) state player) &&
   (check_middlePiece_Equal (get_jump_snd (head jumps)) state player) =
    (jumpToMove (head jumps)):(vJforOnePiece (tail jumps) point state player)
 | otherwise = vJforOnePiece (tail jumps) point state player

-- convert jump to move
jumpToMove :: Jump -> Move
jumpToMove jump = (get_jump_fst jump, get_jump_thd jump)

-- check the point where a peice is moving to is empty
check_moveTo_empty:: Point -> State -> Bool
check_moveTo_empty point state 
 | state == [] = False
 | point == (snd (head state)) =
    if ((fst (head state)) == D)
     then True
     else False
 | otherwise = check_moveTo_empty point (tail state)

-- check the point where a peice is moving to is empty or is an opponent's peice
check_moveTo_EmptyorOppPiece:: Point -> State -> Piece -> Bool
check_moveTo_EmptyorOppPiece point state player
 | state == [] = False
 | point == (snd (head state)) =
    if not ((fst (head state)) == player)
     then True
     else False
 | otherwise = check_moveTo_EmptyorOppPiece point (tail state) player

-- check there is a piece in the middle for a piece to jump over
check_middlePiece_Equal :: Point -> State -> Piece -> Bool
check_middlePiece_Equal point state player
 | state == [] = False
 | point == (snd (head state)) =
    if (fst (head state)) == player
     then True
     else False
 | otherwise = check_middlePiece_Equal point (tail state) player


--
-- generateNewStates
--
-- This function consumes the arguments described below, it first generates a
-- list of valid moves, applies those moves to the current board to generate 
-- a list of next boards, and then checks whether or not that move would 
-- have been possible by filtering out those boards already seen before
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Returns: the list of next boards
--

generateNewStates :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> [Board]
generateNewStates board history grid slides jumps player =
 (statesToBoards (generateNewStates_helper state state_history moves player)) 
 where
 state = boardToState board grid
 state_history = boardsToStates history grid
 moves = moveGenerator state slides jumps player

-- combine grid and board to produce a state
boardToState :: Board -> Grid -> State
boardToState board grid 
 | board == [] = []
 | otherwise = ((head board), (head grid)):(boardToState (tail board) (tail grid))

-- given a list of boards and a grid, convert to a list of states
boardsToStates :: [Board] -> Grid -> [State]
boardsToStates boards grid
 | boards == [] = []
 | otherwise = (boardToState (head boards) grid):(boardsToStates (tail boards) grid)

-- convert a state to the form of a board
stateToBoard :: State -> Board
stateToBoard state
 | state == [] = []
 | otherwise = (fst (head state)):(stateToBoard (tail state))

-- convert a list of states to a list of boards
statesToBoards :: [State] -> [Board]
statesToBoards states
 | states == [] = []
 | otherwise = (stateToBoard (head states)):(statesToBoards (tail states))


-- it is the same as generateNewStates, but instead of consuming slides and jumps
-- it takes in a list of moves which are already generated in generateNewStates.
generateNewStates_helper :: State -> [State] -> [Move] -> Piece -> [State]
generateNewStates_helper state history [] player = []
generateNewStates_helper state history moves player =
 if (elem (generateNewStateForOneMove state (head moves) player) history)
  then (generateNewStates_helper state history (tail moves) player)
  else (generateNewStateForOneMove state (head moves) player):
       (generateNewStates_helper state history (tail moves) player)

-- generate a new board state in accordance with a move.
generateNewStateForOneMove :: State -> Move -> Piece -> State
generateNewStateForOneMove state move player =
 changeStateTo (changeStateFrom state move) move player

-- given a board and a move, change the state of the corresponding tile to
-- empty meaning that a piece has moved out from that tile.
changeStateFrom :: State -> Move -> State
changeStateFrom [] move = []
changeStateFrom state move =
 if ((fst move) == (snd (head state)))
  then (removePiece (head state)):(tail state)
  else (head state):(changeStateFrom (tail state) move)

-- given a board and a move, add a player piece to the correspeonding tile,
-- meaning that the piece has moved to that tile.
changeStateTo :: State -> Move -> Piece -> State
changeStateTo [] move player = []
changeStateTo state move player =
 if ((snd move) == (snd (head state)))
  then ((addPiece (head state) player):(tail state))
  else (head state):(changeStateTo (tail state) move player)

-- remove a piece from the input tile.
removePiece :: Tile -> Tile
removePiece (piece, point) = (D, point)

-- add a player peice to the input tile.
addPiece :: Tile -> Piece -> Tile
addPiece (piece, point) player = (player, point)



 

