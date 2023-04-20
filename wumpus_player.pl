% auxiliary initial action generating rule
act(Action, Knowledge) :-
  % to avoid looping on act/2.
	not(gameStarted),
	assert(gameStarted),
	assert(actionType([])),
  % initial knowledge
	worldSize(X,Y),
	assert(myWorldSize(X,Y)),
	assert(myPosition(1, 1, east)),
  % infomrations about encoutered fields
	assert(myTrail([])),
  	assert(wumpusTiles([])),
  	assert(visitedTiles([[1,1]])),
  % other information
	assert(haveGold(0)),
  	assert(arrows(1)),
  	assert(wumpusAlive(1)),
	act(Action, Knowledge).
% standard action generating rules

/* ACTION priority */

act(Action, Knowledge) :- exit_if_home(Action, Knowledge). %if at home with gold
act(Action, Knowledge) :- return_to_entrance(Action, Knowledge). %if have gold elsewhere
act(Action, Knowledge) :- pick_up_gold(Action, Knowledge). %if just found gold
act(Action, Knowledge) :- sense_stench(Action, Knowledge). %if stench
act(Action, Knowledge) :- move_on(Action,Knowledge). % explore towards new tiles
act(Action, Knowledge) :- else_backtrack(Action, Knowledge). %go back a move
act(Action, Knowledge) :- give_up(Action, Knowledge). % cant do anything else (and im also at 1,1 so i cant backtrack)

/*************************************************/
/* exit if home rules						     */

exit_if_home(Action, Knowledge) :-
	haveGold(G), G > 0,
	myPosition(1, 1, Orient),

	Action = exit,

	Knowledge = [actionType("exit if home with gold")].

/*************************************************/
/* return to entrance rules					     */

return_to_entrance(Action, Knowledge) :-

	haveGold(G), G > 0,
	myWorldSize(Max_X, Max_Y),
	myTrail(Trail),

	Trail = [ [grab,X,Y,Orient] | Trail_Tail],
	New_Trail = [ [turnRight,X,Y,Orient] | Trail_Tail ], %Orient is misleading here

	Action = turnLeft,
	Knowledge = [
				gameStarted,
				actionType("go back after grab"),
				myWorldSize(Max_X, Max_Y),
				myPosition(X, Y, Orient),
				myTrail(New_Trail),
				haveGold(G)
				].

return_to_entrance(Action, Knowledge) :-

	haveGold(G), G > 0,
	myWorldSize(Max_X, Max_Y),
	myTrail([ [Action,X,Y,Orient] | Trail_Tail ]),
	Action = moveForward,
	add_to_list([X,Y], VisitedTiles, New_VisitedTiles),

	Knowledge = [
		gameStarted,
		actionType("go back forward"),
		myWorldSize(Max_X, Max_Y),
		myPosition(X, Y, Orient),
		myTrail(Trail_Tail),
		haveGold(G)
		].

return_to_entrance(Action, Knowledge) :- return_to_entrance_turn(Action, Knowledge).

return_to_entrance_turn(Action, Knowledge) :-

	haveGold(G), G > 0,
	myWorldSize(Max_X, Max_Y),
	myTrail([ [OldAct,X,Y,Orient] | Trail_Tail ]),

	((OldAct=turnLeft,Action=turnRight);(OldAct=turnRight,Action=turnLeft)),
	
	Knowledge = [
		gameStarted,
		actionType("go back turn"),
		myWorldSize(Max_X, Max_Y),
		myPosition(X, Y, Orient),
		myTrail(Trail_Tail),
		haveGold(G)
		].

/*************************************************/
/* pick up gold rules     					     */

pick_up_gold(Action, Knowledge) :-
	glitter,

	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	myTrail(Trail),
	wumpusTiles(WumpusTiles),
	visitedTiles(VisitedTiles),
	haveGold(G),
	arrows(A),
	wumpusAlive(WA),

	Action = grab,
	New_G is G + 1,
	New_Trail = [ [Action,X,Y,Orient] | Trail ],

	Knowledge = [
				gameStarted,
				actionType("pick up gold"),
				myWorldSize(Max_X, Max_Y),
				myPosition(X, Y, Orient),
				myTrail(New_Trail),
				wumpusTiles(WumpusTiles),
				visitedTiles(VisitedTiles),
				haveGold(New_G),
				arrows(A),
				wumpusAlive(WA)
				].
/*************************************************/
/* stench rules     					     */

% wumpus remains alive but i know 2 tiles where it might be
sense_stench(Action, Knowledge) :-
	stenchEventStart,
	stench,
	not(breeze),
	not(scream),

	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	myTrail(Trail),
	wumpusTiles(WumpusTiles),
	visitedTiles(VisitedTiles),
	haveGold(G),
	arrows(A),
	wumpusAlive(WA),
    
	shiftOrientL(Orient, WumpusLDir),
	shiftOrientR(Orient, WumpusRDir),
	forwardStep(X, Y, WumpusLDir, WumpusLX, WumpusLY),
	forwardStep(X, Y, WumpusRDir, WumpusRX, WumpusRY),

	(((\+ member([WumpusLX,WumpusLY],VisitedTiles)) -> New_WumpusTiles = [[WumpusLX,WumpusLY]]); New_WumpusTiles = []),
	(((\+ member([WumpusRX,WumpusRY],VisitedTiles)) -> New_WumpusTiles2 = [[WumpusRX,WumpusRY]|New_WumpusTiles]); New_WumpusTiles2 = New_WumpusTiles),

	Action = moveForward,
	forwardStep(X, Y, Orient, New_X, New_Y),
	add_to_list( [New_X,New_Y], VisitedTiles, New_VisitedTiles),
	New_Trail = [ [Action,X,Y,Orient] | Trail ],

	Knowledge = [
		gameStarted,
		actionType("shoot -> not scream"),
		myWorldSize(Max_X, Max_Y),
		myPosition(New_X, New_Y, Orient),
		myTrail(New_Trail),
		wumpusTiles(New_WumpusTiles2),
		visitedTiles(New_VisitedTiles),
		haveGold(G),
		arrows(A),
		wumpusAlive(New_WA)
		].

% killed wumpus
sense_stench(Action, Knowledge) :-
	stenchEventStart,
	stench,
	not(breeze),
	scream,

	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	myTrail(Trail),
	wumpusTiles(WumpusTiles),
	visitedTiles(VisitedTiles),
	haveGold(G),
	arrows(A),
	wumpusAlive(WA), New_WA is WA - 1, 

	Action = moveForward,
	forwardStep(X, Y, Orient, New_X, New_Y),
	add_to_list( [New_X,New_Y], VisitedTiles, New_VisitedTiles),
	New_Trail = [ [Action,X,Y,Orient] | Trail ],

	Knowledge = [
		gameStarted,
		actionType("shoot -> scream"),
		myWorldSize(Max_X, Max_Y),
		myPosition(New_X, New_Y, Orient),
		myTrail(New_Trail),
		wumpusTiles(WumpusTiles),
		visitedTiles(New_VisitedTiles),
		haveGold(G),
		arrows(A),
		wumpusAlive(New_WA)
		].

% sense stench for the first time and not against wall
sense_stench(Action, Knowledge) :-

	stench,
	not(breeze),
	
	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	not(againstWall(X, Y, Orient, Max_X, Max_Y)),

	myTrail(Trail),
	wumpusTiles(WumpusTiles),
	visitedTiles(VisitedTiles),
	haveGold(G),
	arrows(A), A > 0,
	wumpusAlive(WA),

	Action = shoot,
	New_A is A - 1,
	Knowledge = [
		stenchEventStart,
		gameStarted,
		actionType("sense stench -> shoot"),
		myWorldSize(Max_X, Max_Y),
		myPosition(X, Y, Orient),
		myTrail(Trail),
		wumpusTiles(WumpusTiles),
		visitedTiles(VisitedTiles),
		haveGold(G),
		arrows(New_A),
		wumpusAlive(WA)
		].

% sense stench for the first time and against the wall
sense_stench(Action, Knowledge) :-

	stench,
	not(breeze),
	
	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	myTrail(Trail),
	wumpusTiles(WumpusTiles),
	visitedTiles(VisitedTiles),
	haveGold(G),
	arrows(A), A > 0,
	wumpusAlive(WA),

	forwardStep(X, Y, Orient, New_X, New_Y),  % check if tile in front was visited or its wall
	(not(againstWall(X, Y, Orient, Max_X, Max_Y)); \+ member([New_X, New_Y], VisitedTiles)),  % allows for turning 270 degrees

	Action = turnLeft,
	shiftOrientL(Orient, New_Orient),
	New_Trail = [ [Action,X,Y,Orient] | Trail ],

	Knowledge = [
		gameStarted,
		actionType("sense_stench -> turn because wall/visited"),
		myWorldSize(Max_X, Max_Y),
		myPosition(X, Y, New_Orient),
		myTrail(New_Trail),
		wumpusTiles(WumpusTiles),
		visitedTiles(VisitedTiles),
		haveGold(G),
		arrows(A),
		wumpusAlive(WA)
		].

/*************************************************/
/* move on rules							     */

/* move after backtracking  				     */
% turns saved to knowledge are the other way around
% when turning also flip orient

% field in front not visited/doesnt have wumpus and no breeze
move_on(Action, Knowledge) :-

	backtracking,
	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	visitedTiles(VisitedTiles),
 	wumpusTiles(WumpusTiles),
	forwardStep(X, Y, Orient, New_X, New_Y),
	New_X > 0, New_X =< Max_X, New_Y > 0, New_Y =< Max_Y, % field is in the world
	\+ member([New_X, New_Y], VisitedTiles), % field in front not visited
	(   WumpusTiles \= []
	->  \+ member([New_X, New_Y], WumpusTiles)
	;   true
	),
	myTrail([[OldTurn,X,Y,OldOrient] | Trail_Tail ]),
	haveGold(G),
	arrows(A),
	wumpusAlive(WA),

  	not(breeze),

	((OldTurn=turnLeft,EditedTurn=turnRight);(OldTurn=turnRight,EditedTurn=turnLeft)),
	Edited_Trail = [ [EditedTurn,X,Y,OldOrient] | Trail_Tail ],

	Action = moveForward,
	add_to_list( [New_X,New_Y], VisitedTiles, New_VisitedTiles),
	New_Trail = [ [Action,X,Y,Orient] | Edited_Trail ],

	Knowledge = [
				gameStarted,
				actionType("move on -> forward"),
				myWorldSize(Max_X, Max_Y),
				myPosition(New_X, New_Y, Orient),
				myTrail(New_Trail),
				wumpusTiles(WumpusTiles),
				visitedTiles(New_VisitedTiles),
				haveGold(G),
				arrows(A),
				wumpusAlive(WA)
				].

% turn left after backtracking
move_on(Action, Knowledge) :-
	backtracking,
	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	visitedTiles(VisitedTiles),
	wumpusTiles(WumpusTiles),
	shiftOrientL(Orient, New_Orient),
	forwardStep(X, Y, New_Orient, New_X, New_Y),
	New_X > 0, New_X =< Max_X, New_Y > 0, New_Y =< Max_Y, % field is in the world
	\+ member([New_X, New_Y], VisitedTiles), % field in front not visited
	(   WumpusTiles \= []
	->  \+ member([New_X, New_Y], WumpusTiles)
	;   true
	),
	myTrail(Trail),
	haveGold(G),
	arrows(A),
	wumpusAlive(WA),

 	not(breeze),

	Action = turnLeft,
	((Action=turnLeft,SavedAction=turnRight);(Action=turnRight,SavedAction=turnLeft)),
	((Orient=north,SavedOrient=south);(Orient=south,SavedOrient=north);
	(Orient=east,SavedOrient=west);(Orient=west,SavedOrient=east)),
	New_Trail = [ [SavedAction,X,Y,SavedOrient] | Trail ],

	Knowledge = [
				gameStarted,
				actionType("after backtracking -> move on -> turn left"),
				myWorldSize(Max_X, Max_Y),
				myPosition(X, Y, New_Orient),
				myTrail(New_Trail),
				wumpusTiles(WumpusTiles),
				visitedTiles(VisitedTiles),
				haveGold(G),
				arrows(A),
				wumpusAlive(WA)
				].

% turn right after backtracking
move_on(Action, Knowledge) :-
	backtracking,
	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	visitedTiles(VisitedTiles),
  	wumpusTiles(WumpusTiles),
	shiftOrientR(Orient, New_Orient),
	forwardStep(X, Y, New_Orient, Tile_X, Tile_Y),
	Tile_X > 0, Tile_X =< Max_X, Tile_Y > 0, Tile_Y =< Max_Y, % field is in the world
	\+ member([Tile_X, Tile_Y], VisitedTiles), % field to the right not visited
	(   WumpusTiles \= []                      % field to the right doesnt have wumpus
	->  \+ member([Tile_X, Tile_Y], WumpusTiles)
	;   true
	),
	myTrail(Trail),
	haveGold(G),
	arrows(A),
	wumpusAlive(WA),

  	not(breeze),

	Action = turnRight,
	((Action=turnLeft,SavedAction=turnRight);(Action=turnRight,SavedAction=turnLeft)),
	((Orient=north,SavedOrient=south);(Orient=south,SavedOrient=north);
	(Orient=east,SavedOrient=west);(Orient=west,SavedOrient=east)),
	New_Trail = [ [SavedAction,X,Y,SavedOrient] | Trail ],

	Knowledge = [
				gameStarted,
				actionType("after backtracking -> move on -> turn right"),
				myWorldSize(Max_X, Max_Y),
				myPosition(X, Y, New_Orient),
				myTrail(New_Trail),
				wumpusTiles(WumpusTiles),
				visitedTiles(VisitedTiles),
				haveGold(G),
				arrows(A),
				wumpusAlive(WA)
				].

/* move when not backtracking  				    */

% field in front not visited/doesnt have wumpus and no breeze
move_on(Action, Knowledge) :-

	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	visitedTiles(VisitedTiles),
 	wumpusTiles(WumpusTiles),
	forwardStep(X, Y, Orient, New_X, New_Y),
	New_X > 0, New_X =< Max_X, New_Y > 0, New_Y =< Max_Y, % field is in the world
	\+ member([New_X, New_Y], VisitedTiles), % field in front not visited
	(   WumpusTiles \= []
	->  \+ member([New_X, New_Y], WumpusTiles)
	;   true
	),
	myTrail(Trail),
	haveGold(G),
	arrows(A),
	wumpusAlive(WA),

  not(breeze),

	Action = moveForward,
	add_to_list( [New_X,New_Y], VisitedTiles, New_VisitedTiles),
	New_Trail = [ [Action,X,Y,Orient] | Trail ],

	Knowledge = [
				gameStarted,
				actionType("move on -> forward"),
				myWorldSize(Max_X, Max_Y),
				myPosition(New_X, New_Y, Orient),
				myTrail(New_Trail),
				wumpusTiles(WumpusTiles),
				visitedTiles(New_VisitedTiles),
				haveGold(G),
				arrows(A),
				wumpusAlive(WA)
				].

% field in front visited/has wumpus -> turn left
move_on(Action, Knowledge) :-
	
	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	visitedTiles(VisitedTiles),
	wumpusTiles(WumpusTiles),
	shiftOrientL(Orient, New_Orient),
	forwardStep(X, Y, New_Orient, New_X, New_Y),
	New_X > 0, New_X =< Max_X, New_Y > 0, New_Y =< Max_Y, % field is in the world
	\+ member([New_X, New_Y], VisitedTiles), % field in front not visited
	(   WumpusTiles \= []
	->  \+ member([New_X, New_Y], WumpusTiles)
	;   true
	),
	myTrail(Trail),
	haveGold(G),
	arrows(A),
	wumpusAlive(WA),

 	not(breeze),

	Action = turnLeft,
	New_Trail = [ [Action,X,Y,Orient] | Trail ],

	Knowledge = [
				gameStarted,
				actionType("move on -> turn left"),
				myWorldSize(Max_X, Max_Y),
				myPosition(X, Y, New_Orient),
				myTrail(New_Trail),
				wumpusTiles(WumpusTiles),
				visitedTiles(VisitedTiles),
				haveGold(G),
				arrows(A),
				wumpusAlive(WA)
				].

% field in front visited/has wumpus -> turn right
move_on(Action, Knowledge) :-
	
	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	visitedTiles(VisitedTiles),
  	wumpusTiles(WumpusTiles),
	shiftOrientR(Orient, New_Orient),
	forwardStep(X, Y, New_Orient, Tile_X, Tile_Y),
	Tile_X > 0, Tile_X =< Max_X, Tile_Y > 0, Tile_Y =< Max_Y, % field is in the world
	\+ member([Tile_X, Tile_Y], VisitedTiles), % field to the right not visited
	(   WumpusTiles \= []                      % field to the right doesnt have wumpus
	->  \+ member([Tile_X, Tile_Y], WumpusTiles)
	;   true
	),
	myTrail(Trail),
	haveGold(G),
	arrows(A),
	wumpusAlive(WA),

  	not(breeze),

	Action = turnRight,
	New_Trail = [ [Action,X,Y,Orient] | Trail ],

	Knowledge = [
				gameStarted,
				actionType("move on -> turn right"),
				myWorldSize(Max_X, Max_Y),
				myPosition(X, Y, New_Orient),
				myTrail(New_Trail),
				wumpusTiles(WumpusTiles),
				visitedTiles(VisitedTiles),
				haveGold(G),
				arrows(A),
				wumpusAlive(WA)
				].

/*************************************************/
/* backtrack rules							     */

else_backtrack(Action, Knowledge) :-

	(backtrackTurn2;backtracking),
	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	myTrail([[moveForward,FromX,FromY,OppositeOrient] | Trail_Tail ]),
	wumpusTiles(WumpusTiles),
	visitedTiles(VisitedTiles),
	haveGold(G),
	arrows(A),
	wumpusAlive(WA),

	Action = moveForward,
	shiftOrientL(Orient, New_Orient),

	Knowledge = [
		gameStarted,
		backtracking,
		actionType("backtrack"),
		myWorldSize(Max_X, Max_Y),
		myPosition(FromX, FromY, Orient),
		myTrail(Trail_Tail),
		wumpusTiles(WumpusTiles),
		visitedTiles(VisitedTiles),
		haveGold(G),
		arrows(A),
		wumpusAlive(WA)
		].

else_backtrack(Action,Knowledge) :- else_backtrack_turn(Action,Knowledge).

else_backtrack_turn(Action, Knowledge) :-
	backtracking,
	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),( X > 1; Y > 1 ),
	myTrail([ [OldAct,X,Y,TurnOrient] | Trail_Tail ]),
	wumpusTiles(WumpusTiles),
	visitedTiles(VisitedTiles),
	haveGold(G),
	arrows(A),
	wumpusAlive(WA),

	((OldAct=turnLeft,Action=turnRight);(OldAct=turnRight,Action=turnLeft)),
	((OldAct=turnLeft,shiftOrientR(Orient,New_Orient));
	(OldAct=turnRight,shiftOrientL(Orient,New_Orient))),
	Knowledge = [
		gameStarted,
		backtracking,
		actionType("backtrack turn"),
		myWorldSize(Max_X, Max_Y),
		myPosition(X, Y, New_Orient),
		myTrail(Trail_Tail),
		wumpusTiles(WumpusTiles),
		visitedTiles(VisitedTiles),
		haveGold(G),
		arrows(A),
		wumpusAlive(WA)
		].

else_backtrack(Action, Knowledge) :-

	backtrackTurn1,
	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	myTrail(Trail),
	wumpusTiles(WumpusTiles),
	visitedTiles(VisitedTiles),
	haveGold(G),
	arrows(A),
	wumpusAlive(WA),

	Action = turnLeft,
	shiftOrientL(Orient, New_Orient),

	Knowledge = [
		gameStarted,
		backtrackTurn2,
		actionType("backtrack turn 2"),
		myWorldSize(Max_X, Max_Y),
		myPosition(X, Y, New_Orient),
		myTrail(Trail),
		wumpusTiles(WumpusTiles),
		visitedTiles(VisitedTiles),
		haveGold(G),
		arrows(A),
		wumpusAlive(WA)
		].

else_backtrack(Action, Knowledge) :-

	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient), (X > 1; Y > 1),
	myTrail(Trail),
	wumpusTiles(WumpusTiles),
	visitedTiles(VisitedTiles),
	haveGold(G),
	arrows(A),
	wumpusAlive(WA),

	Action = turnLeft,
	shiftOrientL(Orient, New_Orient),

	Knowledge = [
		gameStarted,
		backtrackTurn1,
		actionType("backtrack turn 1"),
		myWorldSize(Max_X, Max_Y),
		myPosition(X, Y, New_Orient),
		myTrail(Trail),
		wumpusTiles(WumpusTiles),
		visitedTiles(VisitedTiles),
		haveGold(G),
		arrows(A),
		wumpusAlive(WA)
		].

/*************************************************/
/* give up rules  					             */

% due to backtrack im already at 1,1
% if cant move to new tile or backtrack, give up
give_up(Action, Knowledge) :-
	myPosition(1,1,Orient),
	Action = exit,
	
	Knowledge = [
		actionType("give up")
		].

/*************************************************/
/* other rules									 */

% check if wall in front of agent
againstWall(X, Y, Orient, Max_X, Max_Y) :- X = Max_X, 	Y = Y1,   	Orient = east.
againstWall(X, Y, Orient, Max_X, Max_Y) :- X = X1, 		Y = Max_Y, 	Orient = north.
againstWall(X, Y, Orient, Max_X, Max_Y) :- X = 1,     	Y = Y1, 	Orient = west.
againstWall(X, Y, Orient, Max_X, Max_Y) :- X = X1,     	Y = 1,   	Orient = south.

% turn left
shiftOrientL(east, north).
shiftOrientL(north, west).
shiftOrientL(west, south).
shiftOrientL(south, east).

% turn right
shiftOrientR(east, south).
shiftOrientR(north, east).
shiftOrientR(west, north).
shiftOrientR(south, west).

% position after moving forward
forwardStep(X, Y, east,  New_X, Y) :- New_X is (X+1).
forwardStep(X, Y, south, X, New_Y) :- New_Y is (Y-1).
forwardStep(X, Y, west,  New_X, Y) :- New_X is (X-1).
forwardStep(X, Y, north, X, New_Y) :- New_Y is (Y+1).

% add elem to list if not a part of it
add_to_list(Elem, List, Result) :-
  member(Elem, List),
  Result = List.
add_to_list(Elem, List, Result) :-
  \+ member(Elem, List),
  Result = [Elem|List].