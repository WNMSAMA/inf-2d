-- Inf2d Assignment 1 2018-2019
-- Matriculation number:s1742667
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy)
import Debug.Trace
import TTTGame

gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6



{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the  13th March 2018 at 3pm.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]

badNodesList::[Node]
-- This is your list of bad nodes. You should experimet with it to make sure your algorithm covers different cases.
badNodesList = [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6)]

-- The maximum depth this search can reach
-- TODO: Fill in the maximum depth and justify your choice
maxDepth::Int
maxDepth= 35 - length badNodesList
-- Why did you choose this number?
-- YOUR ANSWER GOES HERE
-- Since the gird has the size 6x6, We have 36 blocks to go, thus the maximum depth should ensure
-- the robot can arrive all the blocks in each games. And also we have badNodesList which
-- the robot can't arrive, thus the maxDepth will not consider the bad Nodes, so the
-- maxDepth will be 36-1(robot start nodes) -- length badNodesList (number of bad nodes).



-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.

next::Branch -> [Branch]
next [] =  []
next branch@((a,b):xs) = [(k,y):branch | (k,y)<-filter valid (position(a,b))]
-- function position returns all the possible position the node can go, and function valid checks whether this position is valid.
-- then we use filter to delete invalid node, and use list Comprehension to add the valid node to branch.
-- branch@ means let ((a,b):xs) = branch
  where
    valid (a,b) = (a,b) `notElem` branch && (a,b) `notElem` badNodesList && a<=6 && b>=1 && a>=1 && b<=6
    position(a,b) =[(a+1,b),(a-1,b),(a,b+1),(a,b-1)]

-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
 -- Note that this is the right type declaration for this function. You might have an old version of the Assignment PDF that names this wrongly.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode = destination == curNode


-- Section 3 Uniformed badNodesListSearch
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.

breadthFirstSearch::Node->(Branch -> [Branch])->[Branch]->[Node]->Maybe Branch
-- Branches is the same as froniter in the pesudo-code

breadthFirstSearch destination next branches exploredList
  | null branches = Nothing --If Empty (froniter) return  failure
  -- take the last branch from froniter, check whether it reach the goal state. if so, return the hole branch.
  | checkArrival destination $ head $ last branches = Just$ last branches
  -- use recursion as loop (for each action in problem.Actions do)
  -- We take the last branch from the branches, and find its next position, use checkValid function to
  -- check whether its next position is valid, if is, we put it into head of branches.
  -- Clearly the froniter is a FIFO queue.
  | otherwise = breadthFirstSearch destination next
                ((checkValid (next (last branches)))++(init branches)) (head(last branches):exploredList)
    where
      -- checkValid function use recursion to check whether the branch is valid.
      checkValid :: [Branch]-> [Branch]
      checkValid [] = []
      checkValid (branch:rest)
          -- if child.state is in explored or froniter then we throw it away.
         | (head branch) `elem` ((map head) $ init branches) || head branch `elem` exploredList = checkValid rest
         | otherwise = branch : checkValid rest


-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.


depthFirstSearch::Node->(Branch -> [Branch])->[Branch]-> [Node]-> Maybe Branch
-- In depthFirstSearch we use LIFO queue, thus we take the head of the queue
-- and also put the new element into head of the queue. Other codes will not change at all.
depthFirstSearch destination next branches exploredList
  | null branches = Nothing
  | checkArrival destination $ head $ head branches = Just $ head branches
  | otherwise = depthFirstSearch destination next
              ((checkValid (next (head branches)))++(tail branches)) (head(head branches):exploredList)
    where
      checkValid :: [Branch]-> [Branch]
      checkValid [] = []
      checkValid (branch:rest)
        | (head branch) `elem` ((map head) $ tail branches) || head branch `elem` exploredList = checkValid rest
        | otherwise = branch : checkValid rest



-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.

-- In depthLimitedSearch we use length of the branch as reference of the limit,
-- since if the the depth-limit is x, then the robot can move at most x+1 steps, and
-- all the moves will be recorded in branch.

depthLimitedSearch::Node->(Branch -> [Branch])->[Branch]-> Int-> Maybe Branch
depthLimitedSearch _ _ [] _ = Nothing -- if branches(froniter) is empty then return failure/cut-off.
depthLimitedSearch destination next (b:bs) d -- use b:bs as branches
  | checkArrival destination $ head b = Just b
  | d == length b - 1 = depthLimitedSearch destination next bs d
  -- Since it is depthLimitedSearch, it is a fifo queue, thus we take first element, and put new element into the head.
  | otherwise =  depthLimitedSearch destination next (next b ++ bs) d



-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
-- Each time a solution is not found the depth should be increased.
iterDeepSearch:: Node-> (Branch -> [Branch])->Node -> Int-> Maybe Branch
iterDeepSearch destination next initialNode d
  -- if d is greater than maxDepth then failure.
  | d >= maxDepth = Nothing
  -- We use d as the depth-limit and call depthLimitedSearch function,
  -- if we get nothing then we increase the value of D.
  | searchD == Nothing = iterDeepSearch destination next initialNode (d+1)
  | otherwise = searchD
  where
    searchD = depthLimitedSearch destination next [[initialNode]] d



-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the 'position' point and the 'destination'.

manhattan::Node->Node->Int
-- (a,b) is the start position, (c,d) is the destination
manhattan (a,b) (c,d) = abs(a-c) +abs(b-d)

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

bestFirstSearch::Node->(Branch -> [Branch])->(Node->Int)->[Branch]-> [Node]-> Maybe Branch
bestFirstSearch _ _ _ [] _ = Nothing
bestFirstSearch destination next heuristic (b:bs) exploredList -- Use b:bs as branches
  | checkArrival destination (head b) = Just b
  | head b `elem` exploredList = bestFirstSearch destination next heuristic bs exploredList
  -- Each time we used the next function to the head of the queue, we sort the hole queue by using order function.
  | otherwise = bestFirstSearch destination next heuristic (order ((next b) ++ bs)) ((head b):exploredList)
    where
      -- order takes a branch and sort from low to hgh to the node inside by compare the distance from each node to the destination
      order branch = sortBy (\(a:as) (b:bs) -> compare (heuristic a) (heuristic b)) branch

-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

aStarSearch::Node->(Branch -> [Branch])->(Node->Int)->(Branch ->Int)->[Branch]-> [Node]-> Maybe Branch
-- In aStarearch, we just need modify the order function, from just compare the heuristic to compare
-- heuristic + cost. Which is exactly same as f(n) = h(n)+g(n)
aStarSearch _ _ _ _ [] _ = Nothing
aStarSearch destination next heuristic cost (b:bs) exploredList
  | checkArrival destination (head b) = Just b
  | head b `elem` exploredList = aStarSearch destination next heuristic cost bs exploredList
  | otherwise = aStarSearch destination next heuristic cost (new_order((next b) ++ bs)) ((head b):exploredList)
    where
      new_order branch = sortBy (\a b -> compare ((cost a) + (heuristic (head a))) ((cost b) + (heuristic (head b)))) branch


-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch  -> Int
cost branch = (length branch) - 1


-- | Section 5: Games
-- See TTTGame.hs for more detail on the functions you will need to implement for both games' minimax and alphabeta searches.



-- | Section 5.1 Tic Tac Toe


-- | The eval function should be used to get the value of a terminal state.
-- A positive value (+1) is good for max player. The human player will be max.
-- A negative value (-1) is good for min player. The computer will be min.
-- A value 0 represents a draw.

eval :: Game -> Int
-- simply checks if player 1 has won, and if so returns 1, else check for player 0 and if so returns -1, else returns 0 as draw
eval game
  | checkWin game 1 =  1
  | checkWin game 0 = -1

  | otherwise = 0

-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.

minimax:: Game->Player->Int
minimax game player
  | terminal game = eval game -- If the game is terminal, then we evaluate it.
  -- We use recursion + list comprehension the choose the best value for each max player and min player.
  | player == 1 = maximum [minimax g (switch player)|g<- moves game player]
  | player == 0 = minimum [minimax g (switch player)|g<- moves game player]



-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state.

alphabeta:: Game->Player->Int
alphabeta game player
  -- first we check if current state is terminal.
  | terminal game = eval game
  -- if the it is max player, we called the maxValue function. The set of states will be found by using
  -- moves fucntion. We set alpha(a) to -2,beta(b) to 2,and v to -2.
  -- Similar as min player, but we set v into 2.
  | player == 1  = maxValue (moves game player) (-2) (-2) 2 player
  | player == 0  = minValue (moves game player) (-2) 2 2 player
    where         -- a is alpha and b is beta.
      maxValue [] _ v _ _ = v -- return v if we run out all the game state
      maxValue (g:gs) a v b p
        | terminal g = eval g
        | u >=b = u -- u = max(v,min_value(result(s,a),alpha,beta)).
        | otherwise = maxValue gs (max a u) u b p -- checking the rest of the states.
          where
            u = maximum[v,minValue(moves g (switch p))a 2 b (switch p)]
      -- minValue is very similar to the maxValue function.
      minValue [] _ v _ _ = v
      minValue (g:gs) a v b p
        | terminal g = eval g
        | k <= a = k
        | otherwise = minValue gs a k (minimum[b,k]) p
          where -- k =  min (v,max_value(result(s,a)alpha,beta))
            k = minimum[v,maxValue(moves g (switch p)) a (-2) b (switch p)]
-- | Section 5.2 Wild Tic Tac Toe





-- | The evalWild function should be used to get the value of a terminal state.
-- It should return 1 if either of the move types is in the correct winning position.
-- A value 0 represents a draw.

evalWild :: Game -> Int
-- simply gives the player who reached(!) the terminal state +1  if either the x's or the o's are in the correct position.
evalWild game
  | checkWin game 1 || checkWin game 0 = 1
  | otherwise = 0



-- | The alphabetaWild function should return the minimax value using alphabeta pruning.
-- The evalWild function should be used to get the value of a terminal state. Note that this will now always return 1 for any player who reached the terminal state.
-- You will have to modify this output depending on the player. If a move by the max player sent(!) the game into a terminal state you should give a +1 reward.
-- If the min player sent the game into a terminal state you should give -1 reward.
-- The running time for the first step may take 1.5 minutes.
alphabetaWild:: Game->Player->Int
alphabetaWild game player
  | player == 0  = min_value game (-2) 2 player
  | player == 1  = max_value game (-2) 2 player

max_value :: Game -> Int -> Int -> Player -> Int
max_value g a b player
-- if terminal-Test(state) then return Utility)(state)
-- Since the function max_value are used by player Max, if the max_value
-- reach the final state,that means computer win the game, so if evalWild g==1 ==> -1
  | terminal g && evalWild g == 1 = -1
  | terminal g && evalWild g == 0 = 0
-- call the loop function, for each a in Actions(state))do
  | otherwise = max_value1 (movesWild g player) a (-2) b player


-- Loop for check each state.
max_value1 :: [Game] -> Int -> Int -> Int -> Int -> Int
max_value1 [] _ v _ _ = v
max_value1 (g:gs) a v b p
  | u >= b = u
  | otherwise = max_value1 gs (max a u) u b p
    where
      u = maximum[v,min_value g a b (switch p)]


min_value :: Game -> Int -> Int -> Player -> Int
min_value g a b player
-- if terminal-Test(state) then return Utility)(state)
-- Since the function min_value are used by player min, if the min_value function
-- reach the final state, then it means player win the game, so if evalWild g==1 => 1
  | terminal g && evalWild g == 1 = 1
  | terminal g && evalWild g == 0 = 0
  | otherwise = min_value1 (movesWild g player) a 2 b player

min_value1 :: [Game] -> Int -> Int -> Int -> Int -> Int
min_value1 [] _ v _ _ = v
min_value1 (g:gs) a v b p
  | k <= a = k
  | otherwise = min_value1 gs a k (min b k) p
    where
      k = minimum[v,max_value g a b (switch p)]




-- | End of official assignment. However, if you want to also implement the minimax function to work for Wild Tic Tac Toe you can have a go at it here. This is NOT graded.


-- | The minimaxWild function should return the minimax value of the state (without alphabeta pruning).
-- The evalWild function should be used to get the value of a terminal state.

minimaxWild:: Game->Player->Int
minimaxWild game player =undefined


		 -- | Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms here.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
