structure GameOfLife =
struct

type coordinate = int * int;

datatype state = Alive | Dead;

type relation = coordinate * state;

(* grid is a list of (coordinate, state) tuples, and this is searched element-wise *)

type grid = relation list;

fun cellState((((gx,gy),gstate)::gs):grid, (cx,cy):coordinate) =
  if cx = gx
  then
      if cy = gy
      then gstate
      else cellState(gs,(cx,cy))
  else cellState(gs,(cx,cy))
  | cellState([],_) = Dead;

fun main(program_name, args) =
  1

end;
