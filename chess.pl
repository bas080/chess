color(white).
color(black).

files([a, b, c, d, e, f, g, h]).

file(X) :-
  files(Files),
  member(X, Files).

file_index(File, Index) :-
  files(Files),
  nth0(Index, Files, File).

rank(X) :-
  between(1, 8, X).

piece(X) :-
  member(X, [ queen, king, rook, bishop, knight, pawn ]).

valid_coor(coor(File, Rank)) :-
  file(File),
  rank(Rank).

piece(Piece, Color) :-
  color(Color),
  piece(Piece).

piece(Piece, Color, Coor) :-
  piece(Piece, Color),
  valid_coor(Coor).

valid_piece(piece(Piece, Color, Coor)) :-
  piece(Piece, Color, Coor).

% Movement

direction(diagonal, ne,  1,  1).
direction(diagonal, nw, -1,  1).
direction(diagonal, se,  1, -1).
direction(diagonal, sw, -1, -1).
direction(straight, e,   1,  0).
direction(straight, w,  -1,  0).
direction(straight, n,   0,  1).
direction(straight, s,   0, -1).
pawn_direction(black, diagonal, se).
pawn_direction(black, diagonal, sw).
pawn_direction(black, straight, s).
pawn_direction(white, diagonal, ne).
pawn_direction(white, diagonal, nw).
pawn_direction(white, straight, n).

jump(Angle, Direction, Coor, To) :-
  move(Angle, Direction, Coor, To);
  move(Angle, Direction, Coor, B),
  jump(Angle, Direction, B, To).

move(Angle, Direction, coor(File, Rank), coor(ToFile, ToRank)) :-
  direction(Angle, Direction, FileOffset, RankOffset),
  plus(Rank, RankOffset, ToRank),
  rank(ToRank),
  file_index(File, Index),
  plus(Index, FileOffset, ToIndex),
  file_index(ToFile, ToIndex).

move(Moves, From, To) :-
  todo(Moves, From, To). % We have to check if a pawn has moved. It can jump two squares whenever that is not the case.

move(piece(pawn, C, From), piece(pawn, C, To)) :-
  pawn_direction(C, straight, D),
  move(_, D, From, To).

move(piece(rook, C, From), piece(rook, C, To)) :-
  jump(straight, _, From, To).

move(piece(bishop, C, From), piece(bishop, C, To)) :-
  jump(diagonal, _, From, To).

move(piece(king, C, From), piece(king, C, To)) :-
  move(_, _, From, To).

% The thing with the king and the rook.
move(Moves, MovesOut) :-
  board(Moves, Board),
  todo(Board).

% captures

capture(piece(pawn, C, From), piece(pawn, C, To)) :-
  pawn_direction(C, diagonal, D),
  move(_, D, From, To).

% Also allow capturing a piece when it tries to jump past you.
capture(piece(pawn, C, From), piece(pawn, C, To)) :-
  todo(Moves, From, To).

capture(piece(queen, C, From), piece(queen, C, To)) :-
  jump(_, _, From, To).

capture(piece(rook, C, From), piece(rook, C, To)) :-
  jump(straight, _, From, To).

capture(piece(bishop, C, From), piece(bishop, C, To)) :-
  jump(diagonal, _, From, To).

capture(piece(king, C, From), piece(king, C, To)) :-
  move(_, _, From, To).

valid_option(Piece, capture(Option)) :-
  capture(Piece, Option).

valid_option(Piece, move(Option)) :-
  move(Piece, Option).
