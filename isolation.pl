%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File Name: isolation.pl                                                       %
% ----------------------------------------------------------------------------- %
% Description:                                                                  %
% isolation nXn board game application against the computer, implemented with   %
% AlphaBeta search algorithm.                                                   %
% ----------------------------------------------------------------------------- %
% Synopsis:
% Isolation is a turn-based strategy board game where two players try to confine%
% their opponent on a 7x7 checker-like board.                                   %
% Eventually, they can no longer make a move (thus isolating them) - the player %
% can no longer move loses the game.                                            %
% Each player has one pawn, which they can move around like around like a queen %
% in chess — up/down/left/right/diagonal.                                       %
% There are three conditions under which the pieces can be moved:               %
% 1. They cannot place their piece on an already visited square.                %
% 2. They cannot cross over already visited squares (squeezing through them     %
% diagonally is OK).                                                            %
% 3. They cannot cross over each other’s piece.                                 %
% start playing the game using the predicate play/0.                            %
% after starting the game will start & guide you with additional insructions.   %
% you can choose either playing an interactive game against the computer,       %
% or watching the computer playing against himself.                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          dynamics perdicates & data structures & cleaning the memory:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
% ---------------------------------------------------------------------
% internally used dynamic predicates & data structures:
% ---------------------------------------------------------------------
:- dynamic dimension/1.	% dimension(N): N is the dimension of the nXn quadratic game board.
:- dynamic square/2. % square(Row,Column): the coordinates of a specific square on the game board.
:- dynamic gameBoardSquare/3. % gameBoardSquare(square(Row,Column),Value,BoardIDNumber): the coordinates
% of a specific square & this square's value, on a specific copy of the game board.
:- dynamic nextAvailableBoardIDNumber/1. % nextAvailableBoardIDNumber(NextAvailableBoardIDNumber): board's copies counter.
:- dynamic winner/1. % winner(Player): the winner of the game.
:- dynamic gameEnded/1. % gameEnded(FinalGameBoardIdNumber): the copy of the board where the game ended.
:- dynamic existingPositionEvaluation/4. % existingPositionEvaluation(RealDepthLevel,BoardIndex,GoodPosition,Value): the actual
% depth, the board's index's on the memory, the best position to move to, & this position's value.
:- dynamic userExitedTheGame/0. % userExitedTheGame: user asked to quit the game.
% ---------------------------------------------------------------------
% cleaning the memory from dynamic predicates:
% ---------------------------------------------------------------------
cleanGameMemory:-
  retractall(dimension(_)), % clean the nXn quadratic game board's dimension.
  retractall(square(_,_)), % clean all the coordinates on the game board.
  retractall(gameBoardSquare(_,_,_)), % clean all the squares, their values & all their copies.
  retractall(nextAvailableBoardIDNumber(_)), % clean all the values of the board's copies counter.
  retractall(winner(_)), % clean all the values of winner.
  retractall(gameEnded(_)), % clean all the values of ended game copied of the board.
  retractall(existingPositionEvaluation(_,_,_,_)), % clean all the values of evaluation of positions.
  retractall(userExitedTheGame).  % clean all the values of user asked to quit the game.



/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              game logic & auxiliary predicates for game representation:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
% ---------------------------------------------------------------------
% initializing the game board, including the starting player:
% ---------------------------------------------------------------------
initiateGameBoard(N):-
  assert(dimension(N)), % asserting the board's dimensions in the memory.
  initiateStartPosition(1,1,N), % initiating the game board,
  % starting the top left corner, with BoardIDNumber initialized to 0 (the original board)
  assert(nextAvailableBoardIDNumber(1)). % initiating the number of the next available copy of the board
  % on the copies counter.


% ---------------------------------------------------------------------
% initializing the squares on the game board:
% ---------------------------------------------------------------------
initiateStartPosition(N,N,N):- % stops after initiating all the squares, when the recursion reaches the last square on board.
  assert(gameBoardSquare(square(N,N),0,0)),!. % reached the bottom right corner of the board.

initiateStartPosition(Row,Column,N):-
  ((Row is 1, Column is (N//2)+1, !, Value is 1); % location of x's  player on the starting position.
  (Row is N, Column is (N//2)+1, !, Value is 2); % location of o's  player on the starting position
  Value is 0), % all the rest of the squares on the starting position is empty.
  assert(gameBoardSquare(square(Row,Column),Value,0)), % asserting the game's squares start position value in the memory.
  getNextSquaresCoordinates(Row,Column,NextRow,NextColumn,N), % getting the next coordinates to continue recursivly initiating.
  initiateStartPosition(NextRow,NextColumn,N). % recursivly initiating.

% ---------------------------------------------------------------------
% getting the next row & column coordinates on the board:
% ---------------------------------------------------------------------
getNextSquaresCoordinates(Row,Column,NextRow,NextColumn,N):-
  (Row =< N, Column < N, NextRow is Row, NextColumn is Column+1); % moving through all the columns coordinates on the current row.
  (Row < N, Column is N, NextRow is Row+1, NextColumn is 1). % finished the given row, therefore move to the next row's first column,
  % while validating not movung to a row out of the board's dimension.

% ---------------------------------------------------------------------
% printing the rows & columns coordinates identifiers,the squares & the next turn's player on the given copy of the board:
% ---------------------------------------------------------------------
printCurrentPosition(BoardIDNumber):-
  dimension(N), % get the board's dimension
  nl,
  tab(3), % inaserting 3 spaces so the columns coordinates identifiers will appear exactly above the columns of the board.
  printCurrentPositionColumnsCoordinatesIdentifiers(1,N), % printing the columns coordinates identifiers.
  printCurrentPositionWithRowCoordinatesIdentifiers(1,1,BoardIDNumber,N), nl. % printing the rows coordinates identifiers & the board's squares.


printCurrentPositionWithRowCoordinatesIdentifiers(Row,Column,BoardIDNumber,N):-
  gameBoardSquare(square(Row,Column),Value,BoardIDNumber), % validating the given coordinates has been asserted to the memory.
  ((Value = 0, write('[ ]')); % 0 represents the empty squares on the board.
  (Value = 1, write('[x]')); % 1 represents the square that x player is located on the board.
  (Value = 2, write('[o]')); % 2 represents the square that o player is located on the board.
  (Value = 3, write('[*]'))), % 3 represents the squares already visited on the board.
  (Row is N, Column is N, !, nl); % either last square on the game board - stop the printing, or last index of row or
  % column - continue recursion.
  (getNextSquaresCoordinates(Row,Column,NextRow,NextColumn,N), % getting the next coordinates to continue recursivly printing.
  % printing the rows coordinates identifiers, starting from row 2 (row 1 identifier will be printed later on).
  (NextRow is Row; % when still in the same row identifier, print no identifier.
  (NextRow > Row, !, nl, write('['), write(NextRow), write(']'))), % when moved to a new row, print the new row's identifier.
  printCurrentPositionWithRowCoordinatesIdentifiers(NextRow, NextColumn,BoardIDNumber,N)). % recursivly printing the rows identifiers & the
  % squaers on the given copy of the board.

printCurrentPositionColumnsCoordinatesIdentifiers(Column,N):-
  Column < N+1, write('['), write(Column), write(']'), % when the column's coordinate are smaller than the board's dimension, print the column's identifier.
  ((Column is N, !, nl, write('['), write(1), write(']')); % when the column's coordinate are the same as the board's dimension, go onr row down, & print
  % the first row's identifier.
  (NextColumn is Column + 1, % getting the next column's coordinate to continue recursivly printing.
  printCurrentPositionColumnsCoordinatesIdentifiers(NextColumn,N))). % recursivly printing the columns identifiers.

% ---------------------------------------------------------------------
% printing the start position of the game:
% ---------------------------------------------------------------------
printStartPosition:-
  printCurrentPosition(0).

% ---------------------------------------------------------------------
% duplicating a given game board:
% ---------------------------------------------------------------------
duplicatingGameBoard(BoardIDNumber,NextBoardIDNumber):-
  dimension(N), % get the board's dimension
  retract(nextAvailableBoardIDNumber(NextBoardIDNumber)), % retrieving the current copies counter value.
  NextAvailableBoardIDNumber is NextBoardIDNumber+1, % increasing the copies conter value by 1.
  assert(nextAvailableBoardIDNumber(NextAvailableBoardIDNumber)), % asserting the updated counter value to the memory.
  duplicatingGameBoard(1,1,BoardIDNumber,NextBoardIDNumber,N). % duplicate the board square by square, starting the top left corner/

% overloading:
duplicatingGameBoard(Row,Column,BoardIDNumber,NextBoardIDNumber,N):-
  gameBoardSquare(square(Row,Column),Value,BoardIDNumber), % check that the given coordinates has been asserted to the memory.
  assert(gameBoardSquare(square(Row,Column),Value,NextBoardIDNumber)), % asserting the square on the game board's copy.
  (Row is N, Column is N, !); % either last square on the game board - stop the printing, or last index of row or
  % column - continue recursion.
  (getNextSquaresCoordinates(Row,Column,NextRow,NextColumn,N), % getting the next coordinates to continue recursivly duplicate.
  duplicatingGameBoard(NextRow, NextColumn,BoardIDNumber,NextBoardIDNumber,N)). % recursivly duplicating.

% ---------------------------------------------------------------------
% calculating all the possible directions to move between the squares on the board:
% according to the game's ruls, the pawn can be moved around like a queen in chess — up/down/left/right/diagonal.
% ---------------------------------------------------------------------
getNextDirectionCoordinates(Row,Column,NextRow,NextColumn,Direction):-
  % column directions:
 ((Direction = upColumn, NextRow is Row-1, NextColumn is Column); % moving up.
 (Direction = downColumn, NextRow is Row+1, NextColumn is Column); % moving down.
 % row directions:
 (Direction = leftRow, NextRow is Row, NextColumn is Column-1); % moving to the left.
 (Direction = rightRow, NextRow is Row, NextColumn is Column+1); % moving to the right.
 % cross directions:
 (Direction = leftupDiagonal, NextRow is Row-1, NextColumn is Column-1); % moving to the left up diagonal.
 (Direction = leftDownDiagonal, NextRow is Row+1, NextColumn is Column-1); % moving to the left down diagonal.
 (Direction = rightUpDiagonal, NextRow is Row-1, NextColumn is Column+1); % moving to the left up diagonal.
 (Direction = rightDownDiagonal, NextRow is Row+1, NextColumn is Column+1)). % moving to the left up diagonal.

% ---------------------------------------------------------------------
% a list holds all the  possible directions to move the squares on the
% board game:
% ---------------------------------------------------------------------
possibleDirectionsToMoveList([upColumn,downColumn,leftRow,rightRow,leftupDiagonal,leftDownDiagonal,rightUpDiagonal,rightDownDiagonal]).

% ---------------------------------------------------------------------
% getting a list of all possible legal successor squares of current player's square:
% ---------------------------------------------------------------------
getLegalNextSquaresCoordinates(BoardIDNumber,Player,LegalSquaresCoordinatesList):-
  setof((Row,Column), (gameBoardSquare(square(Row,Column),0,BoardIDNumber),validSquaresCoordinates(square(Row,Column),Player,BoardIDNumber)), LegalSquaresCoordinatesList).

% ---------------------------------------------------------------------
% validating if the current player can move his pawn to the requested square:
% ---------------------------------------------------------------------
validSquaresCoordinates(square(RequestedRow,RequestedColumn),Player,BoardIDNumber):-
  gameBoardSquare(square(RequestedRow,RequestedColumn),0,BoardIDNumber), % validating the requested square is empty on the board.
  gameBoardSquare(square(PlayerRow,PlayerColumn),Player,BoardIDNumber), % getting the square that the player's pawn is.
  possibleDirectionsToMoveList(PossibleDirectionsToMoveList), % getting the possible directions for the player to move.
  member(Direction,PossibleDirectionsToMoveList), % undeterministicly choosing a direction.
  getNextDirectionCoordinates(PlayerRow,PlayerColumn,NextRow,NextColumn,Direction), % exploring the chosen direction.
  gameBoardSquare(square(NextRow,NextColumn),0,BoardIDNumber), % validating the next square is empty on the board.
  validSquaresCoordinates(square(NextRow,NextColumn),RequestedRow,RequestedColumn,BoardIDNumber,Direction).


% overloading.
validSquaresCoordinates(square(Row,Column),RequestedRow,RequestedColumn,BoardIDNumber,Direction):-
  (Row is RequestedRow, Column is RequestedColumn, gameBoardSquare(square(Row,Column),0,BoardIDNumber), !); % stop if the requested coordinates equales the current square.
  (getNextDirectionCoordinates(Row,Column,NextRow,NextColumn,Direction), % exploring the chosen direction.
  gameBoardSquare(square(NextRow,NextColumn),0,BoardIDNumber), % getting the value of the next square on the chosen direction.
  validSquaresCoordinates(square(NextRow,NextColumn),RequestedRow,RequestedColumn,BoardIDNumber,Direction)).

% ---------------------------------------------------------------------
% actually making the move for the player:
% ---------------------------------------------------------------------
movePawnLegallyOnBoard(square(Row,Column),pos(Player1,_,BoardIDNumber1),pos(Player2,square(Row,Column),BoardIDNumber2)):-
  gameBoardSquare(square(PlayerRow,PlayerColumn),Player1,BoardIDNumber1), % getting the square that the player's pawn is.
  gameBoardSquare(square(Row,Column),0,BoardIDNumber1), % validating the requested square on the previous copy of the board is empty.
  validSquaresCoordinates(square(Row,Column),Player1,BoardIDNumber1), % validating the current player can move his pawn to the requested square.
  duplicatingGameBoard(BoardIDNumber1,BoardIDNumber2), % duplicating the previous board.
  retract(gameBoardSquare(square(PlayerRow,PlayerColumn),Player1,BoardIDNumber2)), % making the move on the duplicated board (current board).
  retract(gameBoardSquare(square(Row,Column),0,BoardIDNumber2)), % making the move on the duplicated board (current board).
  assert(gameBoardSquare(square(Row,Column),Player1,BoardIDNumber2)), % making the move on the duplicated board (current board).
  assert(gameBoardSquare(square(PlayerRow,PlayerColumn),3,BoardIDNumber2)),!, % making the move on the duplicated board (current board).
  ((Player1 is 1, Player2 is 2) ; (Player1 is 2, Player2 is 1)). % switching the turns between the x's player & o's player, according to the current player.


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    Auxiliary predicates for the alphaBeta algorithm
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
maxToMove((pos(1,_,_))). % player x is maximum player.
minToMove((pos(2,_,_))). % player o is minimum player.

% ---------------------------------------------------------------------
% getting the maximal depth level according to the difficulty, for the alphaBeta algorithm:
% ---------------------------------------------------------------------
getMaxDepthLevelForDifficulty(Difficulty,MaxDepthLevel):-
  (Difficulty = 1, !, MaxDepthLevel = 1); % easy.
  (Difficulty = 2, !, MaxDepthLevel = 3); % medium.
  (Difficulty = 3, !, MaxDepthLevel = 6). % hard.

% ---------------------------------------------------------------------
% returning an index in the memory to the specific copy of the board in
% order to manage it's state in memory:
% ---------------------------------------------------------------------
getIndexOnMemoryForSpecificCopyOfBoard(BoardIDNumber,BoardIndex):-
  dimension(N), % get the board's dimension.
  getIndexOnMemoryForSpecificCopyOfBoard(BoardIDNumber,BoardIDNumberList,1,1,N), % using overloading predicate to create a flat list, containing all the
  % square on the given copy of the board.
  name(BoardIndex,BoardIDNumberList). % converting the list to an index.

% stopping point for the recursion, when reaches the last square on board.
getIndexOnMemoryForSpecificCopyOfBoard(BoardIDNumber,[Value],N,N,N):-
  gameBoardSquare(square(N,N),Value,BoardIDNumber).

% overloading.
getIndexOnMemoryForSpecificCopyOfBoard(BoardIDNumber,[ListHead|ListTail],Row,Column,N):-
  gameBoardSquare(square(Row,Column),ListHead,BoardIDNumber), !, % asserting currnt square as the list's head.
  getNextSquaresCoordinates(Row,Column,NextRow,NextColumn,N), % getting the next coordinates to continue recursivly creating the list.
  getIndexOnMemoryForSpecificCopyOfBoard(BoardIDNumber,ListTail,NextRow,NextColumn,N). % recursivly creating the list.

% ---------------------------------------------------------------------
% getting a list of all possible legal successor positions of Position:
% ---------------------------------------------------------------------
moves(Position,PositionList,CurrentDepthLevel,MaxDepthLevel):-
  CurrentDepthLevel =< MaxDepthLevel, % validating depth is not greater than the maximal depth.
  setof(pos(Player,square(Row,Column),BoardIDNumber), movePawnLegallyOnBoard(square(Row,Column),Position,pos(Player,square(Row,Column),BoardIDNumber)),PositionList).

% ---------------------------------------------------------------------
% getting the value for a node in the search tree, together with the
% huristic function for the alphaBeta algorithm:
% ---------------------------------------------------------------------
staticval(pos(_,_,BoardIDNumber),Value,Difficulty):-
  dimension(N), % get the board's dimension.
  getPlayerNumberOfMoves(BoardIDNumber,1,MaximumMovesCounter), % get the amount of possible legal moves for x.
  getPlayerNumberOfMoves(BoardIDNumber,2,MinimumMovesCounter), % get the amount of possible legal moves for o.
  DefensiveSubtraction is 2*MaximumMovesCounter - MinimumMovesCounter, % DefensiveSubtraction is the huristic value used to play the easy difficulty.
  OffensiveSubtraction is MaximumMovesCounter - 2*MinimumMovesCounter, % OffensiveSubtraction is the huristic value used to play the medium difficulty.
  Ratio = MaximumMovesCounter/(N*N), % calculating the ratio between the number of moves left dor the maximum player & the total amount of squares on the board.
  % deciding which strategy to play according to the ratio value:
  ((Ratio =< 0.5, OffensiveToDefensive is OffensiveSubtraction); % if ratio is smaller or equal to 0.5, play offensive strategy.
  OffensiveToDefensive is DefensiveSubtraction), % if ratio is greater than 0.5, play defensive strategy.
  ((Difficulty is 1, !,  Value is DefensiveSubtraction);
  % when difficulty is easy (difficulty=1), the winning strategy is a defensive one - maximizing the player’s available moves at a weighted cost against those available to the opponent.
  (Difficulty is 2, !, Value is OffensiveSubtraction);
  % when difficulty is medium (difficulty=2), the winning strategy is an offensive one - minimizing the opponent’s available moves at a weighted cost against those available to the player.
  (Difficulty is 3, !, Value is OffensiveToDefensive)).
  % when difficulty is hard (difficulty=2), the winning strategy an offensive to defensive one - applying an aggressive game-play early (offensive), & later switches to a defensive strategy.

% auxiliary predicate for staticval predicate.
getPlayerNumberOfMoves(BoardIDNumber,Player,NumberOfMoves):-
  getLegalNextSquaresCoordinates(BoardIDNumber,Player,LegalSquaresCoordinatesList),!, % get a list of all the possible squares the player can move to.
  length(LegalSquaresCoordinatesList,NumberOfMoves); % get a list's length.
  NumberOfMoves is 0. % if the list is empty, set the legth to 0.

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                             alphaBeta algorithm:
 the algorithm is based on figure 24.5 on the course book, wite added current depth
max depth & difficulty values.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
alphaBeta(CurrentPosition,Alpha,Beta,GoodPosition,Value,CurrentDepthLevel,MaxDepthLevel,Difficulty):-
  RealDepthLevel is MaxDepthLevel-CurrentDepthLevel, % calculating the actual depth level.
  CurrentPosition = pos(_,_,BoardIDNumber), % defining current position.
  getIndexOnMemoryForSpecificCopyOfBoard(BoardIDNumber,BoardIndex), % getting the board's index.
  % validating if the results existence in the memory:
  ((existingPositionEvaluation(RealDepthLevel,BoardIndex,GoodPosition,Value), !); % case 1 - the results were already exist on the memory.
  % case 2 - the results were not already exist on the memory:
  (((nonvar(Alpha), nonvar(Beta)); % validating the values of alpha & beta.
  (Alpha is -999999, Beta is 999999)), % initiating alpha to -infinity & beta to +infinity.
  moves(CurrentPosition,PositionList,CurrentDepthLevel,MaxDepthLevel), !, % getting a list of all possible legal successor positions.
  NewDepthLevel is CurrentDepthLevel+1, % updating the currnt depth level.
  boundedBest(PositionList,Alpha,Beta,GoodPosition,Value,NewDepthLevel,MaxDepthLevel,Difficulty), % calculting the results.
  assert(existingPositionEvaluation(RealDepthLevel,BoardIndex,GoodPosition,Value))); % asserting the results in the memory.
  staticval(CurrentPosition,Value,Difficulty)). % case 3 - this is a leaf on the search tree, therefore calculting directly.


boundedBest([Position|PositionList],Alpha,Beta,GoodPosition,GoodValue,CurrentDepthLevel,MaxDepthLevel,Difficulty):-
	alphaBeta(Position,Alpha,Beta,_,Value,CurrentDepthLevel,MaxDepthLevel,Difficulty),
	goodEnough(PositionList,Alpha,Beta,Position,Value,GoodPosition,GoodValue,CurrentDepthLevel,MaxDepthLevel,Difficulty).

goodEnough([],_,_,Position,Value,Position,Value,_,_,_):- !.	% No other candidate.

goodEnough(_,Alpha,Beta,Position,Value,Position,Value,_,_,_):-
	minToMove(Position), Value > Beta, ! % Maximizer attained upper bound.
	;
	maxToMove(Position), Value < Alpha, !. % Minimizer attained lower bound.

goodEnough(PositionList,Alpha,Beta,Position,Value,GoodPosition,GoodValue,CurrentDepthLevel,MaxDepthLevel,Difficulty):-
	newBounds(Alpha,Beta,Position,Value,NewAlpha,NewBeta),   % Refine bounds.
	boundedBest(PositionList,NewAlpha,NewBeta,Position1,Value1,CurrentDepthLevel,MaxDepthLevel,Difficulty),
	betterOf(Position,Value,Position1,Value1,GoodPosition,GoodValue).

newBounds(Alpha,Beta,Position,Value,Value,Beta):-
	minToMove(Position),Value > Alpha, !. % Maximizer increased lower bound.

newBounds(Alpha,Beta,Position,Value,Alpha,Value):-
   maxToMove(Position),Value < Beta, !. % Minimizer decreased upper bound.

newBounds(Alpha,Beta,_,_,Alpha,Beta). % Otherwise bounds unchanged.

betterOf(Position,Value,_,Value1,Position,Value):- % Position better than Position1.
	minToMove(Position),Value > Value1, !
	;
	maxToMove(Position), Value < Value1, !.

betterOf(_,_,Position1,Value1,Position1,Value1). % Otherwise Position1 better.



/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Auxiliary predicates for getting data from the user & printing to the screen:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
% ---------------------------------------------------------------------
% appending two lists to one (as leared in claess)::
% ---------------------------------------------------------------------
conc([X|Xs],Ys,[X|Zs]):-
  conc(Xs,Ys,Zs).

conc([],Ys,Ys).

% ---------------------------------------------------------------------
% getting data from the user:
% ---------------------------------------------------------------------
getDataFromUser(UserData):-
  get(DataChar), % getting the first readable character.
  getRestOfData(RestOfData), % getting & proccessing the rest of data line.
  conc([DataChar],RestOfData,UserDataList), % getting the data as a list.
  name(UserData,UserDataList). % converting the user data into a list of the characters ASCII codes.

getRestOfData(DataResult):- % getting & proccessing the rest of data line.
  get0(DataChar), % getting the next character.
  ((DataChar = 10, !, DataResult = []); % stopping when reaching to a new line character.
  (DataChar = 32, !, DataResult = [], stopsGettingDataLine); % use stopsGettingDataLine predicate to stop the reading user dsta when got to the first whithspace.
  (DataResult = [DataChar|RestOfData], getRestOfData(RestOfData))). % continue reading & proccessing

stopsGettingDataLine:- % stop the reading user dsta when got to the first whithspace or to a new line ASCII character.
  get0(DataChar), % getting the next character.
  ((DataChar = 10, !); % stopping when reaching to a new line character.
  stopsGettingDataLine). % stopping when reaching to a whithspace character.

% ---------------------------------------------------------------------
% getting the requaested board's dimension from the user:
% ---------------------------------------------------------------------
getBoardDimensionFromUser(Dimension):-
  nl,
  write('Please enter the requested dimension for the board game, then press ENTER button:'), nl,
  repeat,
  write('Please notice to enter an odd number, equal or greater than 5:'), nl,
  getDataFromUser(Dimension), % getting the dimension from the user.
  ((integer(Dimension), % validating the inserted dimension is an integer.
  Dimension >= 5, 1 is mod(Dimension,2),!); % validating the inserted dimension is according to the game's ruls.
  (printInvalidData(Dimension),fail); % if the dimension is not an integer or not according to the game's ruls, print invalid data massage.
  (userExited(Dimension),!,fail)). % validating if user wants to exit the game.

% ---------------------------------------------------------------------
% getting the requaested game's mode from the user:
% ---------------------------------------------------------------------
getGameModeFromUser(Mode):-
  nl,
  write('Please enter the requested game mode, then press ENTER button:'), nl,
  repeat,
  write('Please notice to enter the number represents the mode you want to play:'), nl,
  write('1 = you & I play against each other, where I start & play the "x", & you play the "o"'), nl,
  write('2 = you & I play against each other, where you start & play the "x", & I play the "o"'), nl,
  write('3 = me playing against myself (an automatic game)'), nl,
  getDataFromUser(Mode), % getting the mode from the user.
  ((integer(Mode), % validating the inserted mode is an integer.
  (Mode >=1, Mode =< 3),!); % validating the inserted mode is according to the game's ruls.
  (printInvalidData(Mode),fail); % if the mode is not an integer or not according to the game's ruls, print invalid data massage.
  (userExited(Mode),!,fail)). % validating if user wants to exit the game.

% ---------------------------------------------------------------------
% getting the requaested game's difficulty from the user:
% ---------------------------------------------------------------------
getGameDifficultyFromUser(Difficulty):-
  nl,
  write('Please choose the difficulty of the game:'), nl,
  repeat,
  write('Please enter a number between 1 to 3, then press ENTER button:'), nl,
  write('1 = easy'), nl,
  write('2 = medium'), nl,
  write('3 = hard'), nl,
  getDataFromUser(Difficulty), % getting the difficulty from the user.
  ((integer(Difficulty), % validating the inserted difficulty is an integer.
  Difficulty >= 1, Difficulty =< 3,!); % validating the inserted difficulty is according to the game's ruls.
  (printInvalidData(Difficulty),fail); % if the difficulty is not an integer or not according to the game's ruls, print invalid data massage.
  (userExited(Difficulty),!,fail)). % validating if user wants to exit the game.

% ---------------------------------------------------------------------
% getting the requested square's coordinates from the user:
% ---------------------------------------------------------------------
getRequestedSquareCoordinatesFromUser(Player,LegalSquaresCoordinatesList,Square):-
  nl,
  write("It's your move now."), nl,
  ((Player = 1, !, write('You need to choose where to place "x" on the board:'));
  (Player = 2, !, write('You need to choose where to place "o" on the board:'))),
  nl,
  repeat,
  write('Please enter the requested square in the (Row,Column) format, then press ENTER button.'), nl,
  write('The possible squares for you are:'), nl,
  write(LegalSquaresCoordinatesList), nl, % printing the optional squares for the player to the screen.
  write('If you would like to stop the game, please type "exit".'), nl,
  getDataFromUser(UserData), % getting the requested square's coordinates from the user.
  % using ASCII's value to validate to user's data:
  % ASCII #40 is opening parenthesis, ASCII #41 is closing parenthesis, ASCII #44 is comma.
  % there are 4 optional cases:
  % case 1 - row's & column's coordinates has one digit each:
  ((name(UserData,[40,X,44,Y,41|_]), name(Row,[X]), name(Column,[Y]), % using name built-in predicate to name the user data as row & column.
  member((Row,Column),LegalSquaresCoordinatesList), !, % validating the requested square's coordinates are part of the possible squaers for the player.
  Square = square(Row,Column)); % defining the square.
  % case 2 - row's coordinate has two digits & column's coordinate has one digit:
  (name(UserData,[40,X,Y,44,Z,41|_]), name(Row,[X,Y]), name(Column,[Z]), % using name built-in predicate to name the user data as row & column.
  member((Row,Column),LegalSquaresCoordinatesList), !, % validating the requested square's coordinates are part of the possible squaers for the player.
  Square = square(Row,Column)); % defining the square.
  % case 3 - row's coordinate has one digit & column's coordinate has two digits:
  (name(UserData,[40,X,44,Y,Z,41|_]), name(Row,[X]), name(Column,[Y,Z]), % using name built-in predicate to name the user data as row & column.
  member((Row,Column),LegalSquaresCoordinatesList), !, % validating the requested square's coordinates are part of the possible squaers for the player.
  Square = square(Row,Column)); % defining the square.
  % case 4 - row's & column's coordinates has two digits each:
  (name(UserData,[40,W,X,44,Y,Z,41|_]), name(Row,[W,X]), name(Column,[Y,Z]), % using name built-in predicate to name the user data as row & column.
  member((Row,Column),LegalSquaresCoordinatesList), !, % validating the requested square's coordinates are part of the possible squaers for the player.
  Square = square(Row,Column)); % defining the square.
  (printInvalidData(UserData),fail); % if the coordinates vaules are not valid values, print invalid data massage.
  (userExited(UserData),!,fail)). % validating if user wants to exit the game.

% ---------------------------------------------------------------------
% getting the user's request to exit the game:
% ---------------------------------------------------------------------
userExited(UserData):-
  nonvar(UserData), % validating the value of user data.
  UserData = "exit", % validating the correct value for exiting.
  assert(userExitedTheGame). % asserting the exiting to the memory.

% ---------------------------------------------------------------------
% printing the starting notes for the game:
% ---------------------------------------------------------------------
printStartNote:-
  write('Hello & wolcome to this ISOLATION game!'), nl,
  sleep(1), % adding a delay between printing the different rows of the note.
  write('This version of the game allows you 3 different game modes, on 3 different difficult levels.'), nl,
  sleep(1), % adding a delay between printing the different rows of the note.
  write('Let us start setting the preferences & play.'), nl,
  sleep(1), % adding a delay between printing the different rows of the note.
  write('Please choose your preferences as instructed below:'), nl,
  sleep(1), % adding a delay between printing the different rows of the note.
  write('Note that if you would like to stop the game & quit, you can type the word "exit" at any time, then press ENTER button'),
  sleep(1). % adding a delay between the printing & starting setting the game.

% ---------------------------------------------------------------------
% printing an invalid data note when user entered invalid data:
% ---------------------------------------------------------------------
printInvalidData(UserData):-
  nl,
  write('OH!,'),
  write(UserData), % printing the invalid user's data.
  write(' is invalid, Please try again.'), nl, nl.

% ---------------------------------------------------------------------
% printing the AI's move:
% ---------------------------------------------------------------------
printAIMove(Row,Column):-
  nl,
  write('AI calculted that my next move will be to ('),
  write(Row), write(','), write(Column), write(').'), nl.% printing the square's coordinates, separated by a comma.

% ---------------------------------------------------------------------
% printing goodbye note when user exits the game, or when the game ends:
% ---------------------------------------------------------------------
printByeBye:-
  write('It was great fun playing with you!'), nl,
  write('Hope to see you again soon!').



/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              Runing the game:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
% ---------------------------------------------------------------------
% main predicate to initiate the complete application - play/0:
% ---------------------------------------------------------------------
play:-
  printStartNote,
  getBoardDimensionFromUser(N), % getting the board's dimension.
  getGameModeFromUser(Mode), % getting the game's mode.
  getGameDifficultyFromUser(Difficulty), % getting the game's difficulty.
  initiateGameBoard(N), % initiating the board according to the dimension.
  printStartPosition, % printing the startin position & starting player.
  % playing the game according to the requested game's mode:
  (((Mode =< 2, % cases 1 & 2 - player vs. the AI:
  playPlayerAgainstAI(Difficulty,Mode,pos(1,_,0))) % use playPlayerAgainstAI predicate to play the game.
  ;
  (Mode is 3, % case 3 - AI vs. the AI:
  playAIAgainstAI(Difficulty,pos(1,_,0)))) % use playAIAgainstAI predicate to play the game.
  ;
  (userExitedTheGame, !, % option 1 for ending the game - user requested to exit the game.
  printByeBye, % printing goodbye note.
  cleanMemoryUp) % cleaning the memory from all asserted items.
  ;
  (gameEnded(_),  % option 2 for ending the game -game ended normally.
  printByeBye, % printing goodbye note.
  cleanMemoryUp)). % cleaning the memory from all asserted items.

% ---------------------------------------------------------------------
% playing player against th AI mode:
% ---------------------------------------------------------------------
playPlayerAgainstAI(Difficulty,Mode,pos(Player1,_,BoardIDNumber)):-
  (getLegalNextSquaresCoordinates(BoardIDNumber,Player1,LegalSquaresCoordinatesList), % validating player 1 still have at list one legal nove.
  BoardIDNumber1 is BoardIDNumber, % defining the BoardIDNumber1 as the currnt board copy.
  ((Mode =\= Player1, !, % current player is o's player - human.
  getRequestedSquareCoordinatesFromUser(Player1,LegalSquaresCoordinatesList,UserSquare), % getting the coordinates from the user.
  movePawnLegallyOnBoard(UserSquare,pos(Player1,_,BoardIDNumber1),pos(Player2,_,BoardIDNumber2)))
  ;
  ( % current player is x's player - AI.
  getMaxDepthLevelForDifficulty(Difficulty,MaxDepthLevel), !, % getting the maximal depth according the chosen difficulty level.
  alphaBeta(pos(Player1,_,BoardIDNumber1),_,_,pos(Player2,square(Row,Column),BoardIDNumber2),_,0,MaxDepthLevel,Difficulty),
  printAIMove(Row,Column))) % printing the AI's move to the screen.
  ,
  % printing the board after the move, & moving the game on with the opposite player.
  (not(gameEnded(_)), % validating both of the players still has possible moves.
  not(userExitedTheGame), % validating user did not exited game.
  ((Player1 is 1, Player2 is 2); (Player1 is 2, Player2 is 1)), %  validating the players.
  nl,
  write('The new board after move is:'),
  printCurrentPosition(BoardIDNumber2), % printing the board after the move.
  sleep(2), % adding a delay between the printing & continuing the game.
  playPlayerAgainstAI(Difficulty,Mode,pos(Player2,_,BoardIDNumber2)))) % continue playing.
  ;
  % game ended - player 1 has no where to moved
  (not(movePawnLegallyOnBoard(square(_,_),pos(Player1,_,BoardIDNumber),pos(Player2,square(_,_),_))), % validating the is no more possible moves for the current player.
  assert(gameEnded(BoardIDNumber)), % asserting the board copy numberd as BoardIDNumber1 as the final board to the memory.
  assert(winner(Player2)), % asserting the winner to the memory.
  ((Player1 is 1, Player2 is 2); (Player1 is 2, Player2 is 1)), %  validating the players.
  write('Game ended!, Player '), write(Player1), write(' has no more moves'), nl,
  write('Player '), write(Player2), write(' wins!')).

% ---------------------------------------------------------------------
% playing AI against th AI mode:
% ---------------------------------------------------------------------
playAIAgainstAI(Difficulty,pos(AI1,_,BoardIDNumber1)):-
  getLegalNextSquaresCoordinates(BoardIDNumber1,AI1,_), !, % validating player AI1 still have at list one legal nove.
  getMaxDepthLevelForDifficulty(Difficulty,MaxDepthLevel), % getting the maximal depth according the chosen difficulty level.
  alphaBeta(pos(AI1,_,BoardIDNumber1),_,_,Position2,_,0,MaxDepthLevel,Difficulty), % getting the best move.
  Position2 = pos(AI2,square(Position2Row,Position2Column),BoardIDNumber2), nl, % setting position 2 as player 2's position.
  write('Computer '), write(AI1), write(' playes ('),  % printing the square's coordinates, separated by a comma.
  write(Position2Row), write(','), write(Position2Column), write(').'),
  nl, nl,
  write('The new board after move is:'),
  printCurrentPosition(BoardIDNumber2), % printing the board after the move.
  sleep(3), % adding a delay between the printing & continuing the game.
  playAIAgainstAI(Difficulty,pos(AI2,_,BoardIDNumber2))
  ;
  % game ended - player 1 has no where to moved:
  assert(gameEnded(BoardIDNumber1)), % asserting the board copy numberd as BoardIDNumber1 as the final board to the memory.
  assert(winner(AI2)), % asserting the winner to the memory.
  ((AI1 is 1, AI2 is 2); (AI1 is 2, AI2 is 1)), %  validating the players.
  write('Game ended!, Player '), write(AI1), write(' has no more moves'), nl,
  write('Player '), write(AI2), write(' wins!').
