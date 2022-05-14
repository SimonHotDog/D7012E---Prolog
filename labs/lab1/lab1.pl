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

% Items:
%
%  S = Steel Key
%  B = Brass Key
%  P = Package

% Movement actions:

proceed(
    state(robot(1, Inventory), Room1, Room2, Room3),
    move(2),
    state(robot(2, Inventory), Room1, Room2, Room3)) :- member(S, Inventory). % Moves from room 1 to room 2

proceed(
    state(robot(2, Inventory), Room1, Room2, Room3),
    move(1),
    state(robot(1, Inventory), Room1, Room2, Room3)) :- member(S, Inventory). % Moves from room 2 to room 1

proceed(
    state(robot(1, Inventory), Room1, Room2, Room3),
    move(3),
    state(robot(3, Inventory), Room1, Room2, Room3)) :- member(B, Inventory). % Moves from room 1 to room 3

proceed(
    state(robot(3, Inventory), Room1, Room2, Room3),
    move(1),
    state(robot(1, Inventory), Room1, Room2, Room3)) :- member(B, Inventory). % Moves from room 3 to room 1