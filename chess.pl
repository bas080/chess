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

direction(n, n, 0, 1).
direction(n, e, 1, 1).
direction(n, w, -1, 1).
direction(s, s, 0, -1).
direction(s, e, 1, -1).
direction(s, w, -1, -1).
direction(w, w, -1, 0).
direction(e, e, 1, 0).

direction(diagonal, D) :-
  valid_direction(D),
  D = direction(A, B, _, _),
  A \= B.

direction(straight, D) :-
  valid_direction(D),
  D = direction(A, A, _, _).

valid_direction(direction(A, B, C, D)) :-
  direction(A, B, C, D).

jump(Direction, Coor, To) :-
  move(Direction, Coor, To);
  move(Direction, Coor, B),
  jump(Direction, B, To).

move(Direction, coor(File, Rank), coor(ToFile, ToRank)) :-
  valid_direction(Direction),
  Direction = direction(_, _, FileOffset, RankOffset),
  plus(Rank, RankOffset, ToRank),
  rank(ToRank),
  file_index(File, Index),
  plus(Index, FileOffset, ToIndex),
  file_index(ToFile, ToIndex).

move(piece(pawn, black, From), piece(pawn, black, To)) :-
  move(direction(s), From, To).

move(piece(pawn, white, From), piece(pawn, white, To)) :-
  move(direction(n), D, From, To).

move(piece(queen, C, From), piece(queen,C, To)) :-
  jump(_, _, From, To).

move(piece(rook, C, From), piece(rook, C, To)) :-
  jump(straight, _, From, To).

move(piece(bishop, C, From), piece(bishop, C, To)) :-
  jump(diagonal, _, From, To).

move(piece(king, C, From), piece(king, C, To)) :-
  move(_, _, From, To).

% captures

capture(piece(pawn, C, From), piece(pawn, C, To)) :-
  pawn_direction(C, diagonal, D),
  move(_, D, From, To).

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
