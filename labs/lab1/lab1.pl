% Simon Lundberg

% This is how I represent a state. The robot has a position, as well as an inventory containing the items it has picked up.
% Every room has [Loot], which contain the items in the room.
%
% state(
%     robot(position, [Inventory]),
%     Room1([Loot]),
%     Room2([Loot]),
%     Room3([Loot])
% )

% Movement actions:

proceed(
    state(robot(1, Inventory), Room1, Room2, Room3), % Moves from room 1 to room 2
    move(2),
    state(robot(2, Inventory), Room1, Room2, Room3)) :- member(steel, Inventory). 

proceed(
    state(robot(2, Inventory), Room1, Room2, Room3), % Moves from room 2 to room 1
    move(1),
    state(robot(1, Inventory), Room1, Room2, Room3)) :- member(steel, Inventory). 

proceed(
    state(robot(1, Inventory), Room1, Room2, Room3), % Moves from room 1 to room 3
    move(3),
    state(robot(3, Inventory), Room1, Room2, Room3)) :- member(brass, Inventory). 

proceed(
    state(robot(3, Inventory), Room1, Room2, Room3), % Moves from room 3 to room 1
    move(1),
    state(robot(1, Inventory), Room1, Room2, Room3)) :- member(brass, Inventory). 

% Pick up item actions:

proceed(
    state(robot(1, Inventory), Pre_Room1, Room2, Room3), % Picks up item in room 1
    pick(Item),
    state(robot(1, [Item|Inventory]), Post_Room1, Room2, Room3)) :- length(Inventory, Len), Len < 2, member(Item, Pre_Room1), delete(Pre_Room1, Item, Post_Room1).

proceed(
    state(robot(2, Inventory), Room1, Pre_Room2, Room3), % Picks up item in room 2
    pick(Item),
    state(robot(2, [Item|Inventory]), Room1, Post_Room2, Room3)) :- length(Inventory, Len), Len < 2, member(Item, Pre_Room2), delete(Pre_Room2, Item, Post_Room2).

proceed(
    state(robot(3, Inventory), Room1, Room2, Pre_Room3), % Picks up item in room 3
    pick(Item),
    state(robot(3, [Item|Inventory]), Room1, Room2, Post_Room3)) :- length(Inventory, Len), Len < 2, member(Item, Pre_Room3), delete(Pre_Room3, Item, Post_Room3). 

% Drop item actions

proceed(
    state(robot(1, Pre_Inventory), Room1, Room2, Room3), % Drops item while in room 1
    drop(Item),
    state(robot(1, Post_Inventory), [Item|Room1], Room2, Room3)) :- member(Item, Pre_Inventory), delete(Pre_Inventory, Item, Post_Inventory).

proceed(
    state(robot(2, Pre_Inventory), Room1, Room2, Room3), % Drops item while in room 2
    drop(Item),
    state(robot(2, Post_Inventory), Room1, [Item|Room2], Room3)) :- member(Item, Pre_Inventory), delete(Pre_Inventory, Item, Post_Inventory).

proceed(
    state(robot(3, Pre_Inventory), Room1, Room2, Room3), % Drops item while in room 3
    drop(Item),
    state(robot(3, Post_Inventory), Room1, Room2, [Item|Room3])) :- member(Item, Pre_Inventory), delete(Pre_Inventory, Item, Post_Inventory).

% Solving the puzzle

solveR(state(_, _, Room2, _), N, [finished | []]) :- member(package, Room2).

solveR(State0, N, [Proceed|T]) :- N > 0, proceed(State0, Proceed, State1), solveR(State1, N - 1, T).

% For testing, run: solveR(state(robot(1, []), [steel], [brass], [package]), N, X).
%
% Minimum N for success is 12