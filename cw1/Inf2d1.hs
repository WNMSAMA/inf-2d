-- Inf2d Assignment 1 2018-2019
-- Matriculation number:s1703367
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
badNodesList = [(3,6)]

-- The maximum depth this search can reach
-- TODO: Fill in the maximum depth and justify your choice
maxDepth::Int
maxDepth=35
-- Why did you choose this number?
-- YOUR ANSWER GOES HERE
--The total number of grids is 36, so the length of the longest branch is 36,
-- therefore the depth of it is 35.

-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.

next::Branch -> [Branch]
next [] =  []
next branch = [n : branch| n <- nodeNext(head branch) , not (n `elem` branch)]

--this function will return all possible nodes of next move.
nodeNext :: Node -> [Node]
nodeNext n = rmInvalidNodes [(fst n + 1,snd n),(fst n - 1,snd n),(fst n,snd n + 1),(fst n,snd n - 1)]
--this function removes nodes in the badNodesList and any nodes out of range of (1,1) and (6,6)
rmInvalidNodes :: [Node] -> [Node]
rmInvalidNodes ns = [n | n <- ns , not(n `elem` badNodesList) && fst n >= 1 && snd n >= 1 && snd n <= 6 && fst n <= 6]



-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
 -- Note that this is the right type declaration for this function. You might have an old version of the Assignment PDF that names this wrongly.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode = destination == curNode


-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.

breadthFirstSearch::Node->(Branch -> [Branch])->[Branch]->[Node]->Maybe Branch

breadthFirstSearch destination next [] exploredList = Nothing
breadthFirstSearch destination next (b:bs) exploredList
  |checkArrival destination (head b) = Just b -- if the current node is the destination , return the branch which the node is in.
  --if the current node is a bad node or have already explored, skip the whole branch.
  |(head b `elem` exploredList) || or [head b `elem` bss| bss <- bs] = breadthFirstSearch destination next bs exploredList
  --otherwise append the expanded branches to the end of the search list.(FIFO)
  |otherwise = breadthFirstSearch destination next (bs ++ next b) (head b :exploredList)



-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.
depthFirstSearch::Node->(Branch -> [Branch])->[Branch]-> [Node]-> Maybe Branch
depthFirstSearch destination next [] exploredList = Nothing
depthFirstSearch destination next (b:bs) exploredList
  |checkArrival destination (head b) = Just b-- if the current node is the destination , return the branch which the node is in.
  --if the current node is a bad node or have already explored, skip the whole branch.
  |(head b `elem` exploredList) || or [head b `elem` bss| bss <- bs] = depthFirstSearch destination next bs exploredList
  --otherwise add the expanded branches to the head of the search list.(LIFO)
  |otherwise = depthFirstSearch destination next (next b ++ bs) (head b :exploredList)

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Node->(Branch -> [Branch])->[Branch]-> Int-> Maybe Branch
depthLimitedSearch destination next [] d = Nothing
depthLimitedSearch destination next (b:bs) d
  |checkArrival destination (head b) = Just b
  --if the depth of a branch has reached the pre-set depth and not reached the destination,skip the whole branch.
  |d == length b -1 = depthLimitedSearch destination next bs d
  --otherwise add the expanded branches to the head of the search list.(LIFO)
  |otherwise = depthLimitedSearch destination next (next b ++ bs) d

-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
-- Each time a solution is not found the depth should be increased.
iterDeepSearch:: Node-> (Branch -> [Branch])->Node -> Int-> Maybe Branch
iterDeepSearch destination next initialNode 35 = Nothing-- set the upperlimit of the depth to 35.
iterDeepSearch destination next initialNode d
  --use depthLimitedSearch, if it can find a path with the current depth, return that branch.
  |depthLimitedSearch destination next [[initialNode]] d /= Nothing = depthLimitedSearch destination next [[initialNode]] d
  --otherwise just add 1 to the current depth and search again.
  |otherwise = iterDeepSearch destination next initialNode (d+1)


-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the 'position' point and the 'destination'.

manhattan::Node->Node->Int
manhattan position destination = abs(fst position - fst destination) + abs(snd position - snd destination)

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

bestFirstSearch::Node->(Branch -> [Branch])->(Node->Int)->[Branch]-> [Node]-> Maybe Branch
bestFirstSearch destination next heuristic [] exploredList= Nothing
bestFirstSearch destination next heuristic (b:bs) exploredList
  |checkArrival destination (head b) = Just b
  |(head b `elem` exploredList) || or [head b `elem` bss| bss <- bs] = bestFirstSearch destination next heuristic bs exploredList
  --use order helper function to sort the branches in ascending order.
  |otherwise = bestFirstSearch destination next heuristic (order (next b ++ bs)) (head b : exploredList)
    where
      --this will sort the branches by compairing there heuristic values.
      order branch = map snd(sortBy (\(x,_) (y,_) -> compare x y) [(heuristic (head sb),sb) | sb <- branch])
-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

aStarSearch::Node->(Branch -> [Branch])->(Node->Int)->(Branch ->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch destination next heuristic cost [] exploredList = Nothing
aStarSearch destination next heuristic cost (b:bs) exploredList
  |checkArrival destination (head b) = Just b
  |(head b `elem` exploredList) || or [head b `elem` bss| bss <- bs] = aStarSearch destination next heuristic cost bs exploredList
  |otherwise = aStarSearch destination next heuristic cost (order (next b ++ bs)) (head b : exploredList)
    where
      --this wil sort the branches by comparing the sum of the heuristic and the cost.
      order branch = map snd(sortBy (\(x,_) (y,_) -> compare x y) [(heuristic (head sb) + cost sb,sb) | sb <- branch])
-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch  -> Int
cost branch = length branch -1 --  the cost is the depth of a branch.


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
  | terminal game && checkWin game 1 = 1
  | terminal game && checkWin game 0 = -1
  | otherwise = 0

-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.

minimax:: Game->Player->Int
minimax game player
  | terminal game = eval game -- if the gameis over , return the utility value
  -- if the current player is human, find the maximum value of the nodes
  --the values of the nodes is depend on the moves of the next player.(computer)
  | player == 1 = maximum [minimax p (switch player) | p <- moves game player ]
  -- if the current player is computer, find the minimum value of the nodes
  --the values of the nodes is depend on the moves of the next player.(human)
  | player == 0 = minimum [minimax p (switch player) | p <- moves game player ]

-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state.


alphabeta:: Game->Player->Int
--let -2 denote -inf and +2 denote +inf
alphabeta game player = maxvalue game (-2) 2
--Following the pseudocode.....
maxvalue :: Game-> Int -> Int ->Int
maxvalue game alpha beta
  | terminal game = eval game-- if Terminal(state) return Utility(state)
  | otherwise = maxab alpha (moves game 1) alpha beta -- start the for loop for human player
   where
     maxab :: Int -> [Game] -> Int -> Int -> Int
     maxab v [] a b = v -- loop ends , return v
     maxab v (g : gs) a b
      | value >= b = value -- if value >= beta then return value
      | otherwise = maxab value gs (max a value) b--else set alpha <- Max(a, value) and continue the loop
        where
          value = max v (minvalue g a b)--value <- Max(v,minvalue((result of human moves) alpha beta))
minvalue :: Game-> Int -> Int ->Int
minvalue game alpha beta
  | terminal game = eval game-- if Terminal(state) return Utility(state)
  | otherwise = minab beta (moves game 0) alpha beta-- start the for loop for computer
    where
      minab :: Int -> [Game] -> Int -> Int -> Int
      minab v [] a b = v-- loop ends , return v
      minab v (g:gs) a b
        | value <= a = value-- if value <= alpha then return value
        | otherwise = minab value gs a (min b value)--else set beta <- Min(beta, value) and continue the loop
          where
            value = min v (maxvalue g a b)--value <- Min(v,maxvalue((result of computer moves) alpha beta))



-- | Section 5.2 Wild Tic Tac Toe





-- | The evalWild function should be used to get the value of a terminal state.
-- It should return 1 if either of the move types is in the correct winning position.
-- A value 0 represents a draw.

evalWild :: Game -> Int
-- simply gives the player who reached(!) the terminal state +1  if either the x's or the o's are in the correct position.
evalWild game
  | terminal game && checkWin game 1 = 1
  | terminal game && checkWin game 0 = 1
  | otherwise = 0



-- | The alphabetaWild function should return the minimax value using alphabeta pruning.
-- The evalWild function should be used to get the value of a terminal state. Note that this will now always return 1 for any player who reached the terminal state.
-- You will have to modify this output depending on the player. If a move by the max player sent(!) the game into a terminal state you should give a +1 reward.
-- If the min player sent the game into a terminal state you should give -1 reward.

--This part will take about 90 seconds to finish running if using ghci.
alphabetaWild:: Game->Player->Int
alphabetaWild game player = maxvalueWild game (-2) 2
--Most part performs silillar to alphabeta.
maxvalueWild :: Game-> Int -> Int ->Int
maxvalueWild game alpha beta
  | terminal game && evalWild game  == 1  = -1 -- if the human reach the terminal state, the utility value is set to -1
  | terminal game && evalWild game  == 0  = 0 -- if draw set the utility value to 0
  | otherwise = maxabWild alpha (movesWild game 1) alpha beta
   where
     maxabWild :: Int -> [Game] -> Int -> Int -> Int
     maxabWild v [] a b = v
     maxabWild v (g : gs) a b
      | value >= b = value
      | otherwise = maxabWild value gs (max a value) b
        where
          value = max v (minvalueWild g a b)
minvalueWild :: Game-> Int -> Int ->Int
minvalueWild game alpha beta
  | terminal game && evalWild game == 1 = 1-- if the computer reach the terminal state, the utility value is set to 1
  | terminal game && evalWild game == 0 = 0-- if draw set the utility value to 0
  | otherwise = minabWild beta (movesWild game 0) alpha beta
    where
      minabWild :: Int -> [Game] -> Int -> Int -> Int
      minabWild v [] a b = v
      minabWild v (g:gs) a b
        | value <= a = value
        | otherwise = minabWild value gs a (min b value)
          where
            value = min v (maxvalueWild g a b)



-- | End of official assignment. However, if you want to also implement the minimax function to work for Wild Tic Tac Toe you can have a go at it here. This is NOT graded.


-- | The minimaxWild function should return the minimax value of the state (without alphabeta pruning).
-- The evalWild function should be used to get the value of a terminal state.

minimaxWild:: Game->Player->Int
minimaxWild game player =undefined


-- | Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms here.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
