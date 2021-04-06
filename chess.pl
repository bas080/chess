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
  rank(Rank),
  file(File).

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

jump_angle(straight, Coor, To) :-
  direction(straight, D),
  jump(D, Coor, To).

jump_angle(diagonal, Coor, To) :-
  direction(diagonal, D),
  jump(D, Coor, To).

jump(Direction, Coor, To) :-
  move(Direction, Coor, To);
  move(Direction, Coor, B),
  jump(Direction, B, To).

move(Direction, coor(File, Rank), coor(ToFile, ToRank)) :-
  valid_direction(Direction),
  Direction = direction(_, _, FileOffset, RankOffset),
  rank(Rank),
  plus(Rank, RankOffset, ToRank),
  rank(ToRank),
  file_index(File, Index),
  plus(Index, FileOffset, ToIndex),
  file_index(ToFile, ToIndex).

move(piece(pawn, black, From), piece(pawn, black, To)) :-
  move(direction(s, s, _, _), From, To).

move(piece(pawn, white, From), piece(pawn, white, To)) :-
  move(direction(n, n, _, _), From, To).

move(piece(queen, C, From), piece(queen,C, To)) :-
  jump(_, From, To).

move(piece(rook, C, From), piece(rook, C, To)) :-
  jump_angle(straight, From, To).

move(piece(bishop, C, From), piece(bishop, C, To)) :-
  jump_angle(diagonal, From, To).

move(piece(king, C, From), piece(king, C, To)) :-
  move(_, From, To).

% captures

capture(piece(pawn, white, From), piece(pawn, white, To)) :-
  direction(diagonal, D),
  D = direction(n, _, _, _),
  move(D, From, To).

capture(piece(pawn, black, From), piece(pawn, black, To)) :-
  direction(diagonal, D),
  D = direction(s, _, _, _),
  move(D, From, To).

capture(piece(queen, C, From), piece(queen, C, To)) :-
  jump_angle(_, From, To).

capture(piece(rook, C, From), piece(rook, C, To)) :-
  jump_angle(straight, From, To).

capture(piece(bishop, C, From), piece(bishop, C, To)) :-
  jump(diagonal, From, To).

capture(piece(king, C, From), piece(king, C, To)) :-
  move(_, From, To).

% Should check if move does not have any pieces in the way.
valid_move(BoardIn, move(Piece, To), Board) :-
  member(Piece, BoardIn),
  move(Piece, To),
  To = piece(_, _, Coor),
  \+ member(piece(_, _, Coor), BoardIn), % check if place is free
  subtract([To|BoardIn], [Piece], Board).

board_piece(pawn, Pawn) :-
  file(F), (
    Pawn = piece(pawn, white, coor(F, 2));
    Pawn = piece(pawn, black, coor(F, 7))
  ).

board_piece(rook, Rook) :-
  (F = a; F = h), (
    Rook = piece(rook, white, coor(F, 1));
    Rook = piece(rook, black, coor(F, 8))
  ).

board_piece(knight, Knight) :-
  (F = b; F = g), (
    Knight = piece(knight, white, coor(F, 1));
    Knight = piece(knight, black, coor(F, 8))
  ).

board_piece(bishop, Bishop) :-
  (F = c; F = f), (
    Bishop = piece(bishop, white, coor(F, 1));
    Bishop = piece(bishop, black, coor(F, 8))
  ).

board_piece(queen, Queen) :-
  Queen = piece(queen, white, coor(d, 1));
  Queen = piece(queen, black, coor(d, 8)).

board_piece(king, King) :-
  King = piece(king, white, coor(e, 1));
  King = piece(king, black, coor(e, 8)).

board(Pieces) :-
  findall(Piece, board_piece(_, Piece), Pieces).

% Pretty Printing boards

piece_unicode(white, king,   "♔").
piece_unicode(white, queen,  "♕").
piece_unicode(white, rook,   "♖").
piece_unicode(white, bishop, "♗").
piece_unicode(white, knight, "♘").
piece_unicode(white, pawn,   "♙").
piece_unicode(black, king,   "♚").
piece_unicode(black, queen,  "♛").
piece_unicode(black, rook,   "♜").
piece_unicode(black, bishop, "♝").
piece_unicode(black, knight, "♞").
piece_unicode(black, pawn,   "♟︎").

board_pp(Board, Print) :-
  findall(Row, (rank(Rank), board_rank_pp(Board, Rank, Row)), Rows),
  atomics_to_string(Rows, '\n', Print).

board_rank_pp(Board, Rank, Print) :-
  findall(Square, (
    file(File),
    board_square_pp(Board, coor(File, Rank), Square)
  ), Squares),
  atomics_to_string(Squares, " ", Print).

board_square_pp(Board, Coor, Print) :-
  member(piece(Type, Color,  Coor), Board)
    -> piece_unicode(Color, Type, Print)
    ;  Print = " ".

