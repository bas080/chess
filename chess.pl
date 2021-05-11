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

% Consider renaming to offset
direction(n, n, vector(0,  1)).
direction(n, e, vector(1,  1)).
direction(n, w, vector(-1, 1)).
direction(s, s, vector(0,  -1)).
direction(s, e, vector(1,  -1)).
direction(s, w, vector(-1, -1)).
direction(w, w, vector(-1, 0)).
direction(e, e, vector(1,  0)).

coor_vector(Coor, vector(Index, Rank)) :-
  valid_coor(Coor),
  Coor = coor(File, Rank),
  file_index(File, Index).

vector_sum(vector(A, B), vector(X, Y), vector(R, Z)) :-
  plus(A, X, R),
  plus(B, Y, Z).

direction(knight, direction(Cardinal, Ordinal, Vector)) :-
  direction(diagonal, direction(Cardinal, Ordinal, V1)),
  ( D = Cardinal; D = Ordinal ),
  direction(D, D, V2),
  vector_sum(V1, V2, Vector).

direction(diagonal, direction(A, B, V)) :-
  direction(A, B, V),
  A \= B.

direction(straight, direction(A, A, V)) :-
  direction(A, A, V).

valid_direction(B) :-
  direction(A, B).

jump(Coor, To, Direction) :-
  neighbors(Coor, To, Direction);
  neighbors(Coor, B, Direction),
  jump(B, To, Direction).

neighbors(From, To, Direction) :-
  valid_direction(Direction),
  Direction = direction(_, _, V1),
  coor_vector(From, V2),
  vector_sum(V1, V2, V3),
  coor_vector(To, V3).

move(piece(knight, C, From), piece(knight, C, To), Direction) :-
  direction(knight, Direction),
  writeln(Direction),
  neighbors(From, To, Direction).

move(piece(pawn, black, From), piece(pawn, black, To), Direction) :-
  Direction = direction(s, s, _),
  neighbors(From, To, Direction).

move(piece(pawn, white, From), piece(pawn, white, To), Direction) :-
  Direction = direction(n, n, _),
  neighbors(From, To, Direction).

move(piece(pawn, white, coor(F, 2)), piece(pawn, white, coor(F, 4)), _).
move(piece(pawn, black, coor(F, 7)), piece(pawn, black, coor(F, 5)), _).

move(piece(queen, C, From), piece(queen,C, To), Direction) :-
  direction(straight, Direction);
  direction(diagonal, Direction),
  jump(From, To, Direction).

move(piece(rook, C, From), piece(rook, C, To), Direction) :-
  direction(straight, Direction),
  jump(From, To, Direction).

move(piece(bishop, C, From), piece(bishop, C, To), Direction) :-
  direction(diagonal, Direction),
  jump(From, To, Direction).

move(piece(king, C, From), piece(king, C, To), Direction) :-
  direction(straight, Direction);
  direction(diagonal, Direction),
  neighbors(From, To, Direction).

intersections(From, Inter, Inter, Direction) :-
  move(From, Inter, Direction).

intersections(From, Inter, To, Direction) :-
  move(From, Inter, Direction),
  move(Inter, To, Direction).

% captures

% Should check if move does not have any pieces in the way.
move(BoardIn, move(Piece, To, Direction), Board) :-
  member(Piece, BoardIn),
  move(Piece, To, Direction),
  writeln('called'),
  \+ ( % check if not intersecting with anything in the line.
    intersections(Piece, piece(_, _, Coor), To, Direction),
    member(piece(_, _, Coor), BoardIn)
  ),
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

