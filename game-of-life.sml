structure GameOfLife =
struct

type coordinate = int * int;

datatype state = Alive | Dead;

type relation = coordinate * state;

type grid = relation list;

fun cellState((((gx,gy),gstate)::gs):grid)((cx,cy):coordinate) =
  if cx = gx
  then
      if cy = gy
      then gstate
      else cellState(gs)((cx,cy))
  else cellState(gs)((cx,cy))
  | cellState([])(_) = Dead;

fun displayState(Alive)= "O" | displayState(Dead) = " ";

fun adjacentCoordinates((cx,cy):coordinate) : coordinate list=
  [((cx - 1),(cy - 1)),
   (cx,(cy - 1)),
   ((cx + 1),(cy - 1)),
   ((cx - 1),cy),
   ((cx + 1),cy),
   ((cx - 1),(cy + 1)),
   (cx,(cy + 1)),
   ((cx + 1),(cy + 1))];

fun cellAlive(Alive) = true
  | cellAlive(Dead) = false;

fun numberOfLiveNeighbours(g:grid, c:coordinate) =
  length
      (List.filter cellAlive
		   (List.map (cellState(g))
			     (adjacentCoordinates(c))));

fun aliveRule(2) = Alive
  | aliveRule(3) = Alive
  | aliveRule(_) = Dead;

fun deadRule(3) = Alive
  | deadRule(_) = Dead;

fun evolveCell(g:grid, c:coordinate) =
  if cellState(g)(c) = Alive
  then
      aliveRule(numberOfLiveNeighbours(g,c))
  else
      deadRule(numberOfLiveNeighbours(g,c));

fun tick(g:grid) =
  List.map (fn (c,m) => (c,evolveCell(g,c))) g;

fun cat((x::xs),ys) =
  x::cat(xs,ys)
  | cat([],ys) = ys;

fun concat((xs::ys::xxs)) = concat(cat(xs,ys)::xxs)
  | concat((xs::[])) = xs;

fun range(start,finish,xs) =
  if start = finish
  then
      xs
  else
      range(start,finish - 1,(finish - 1)::xs)

fun randomState(rng,p) =
  if Random.randReal(rng) < p
  then Alive
  else Dead;

fun coordToRandomState(rng,p)(c:coordinate) =
  (c,randomState(rng,p))

fun displayLine(g:grid, w)(y) =
  let val _ = print(String.concat (map (fn x => displayState(cellState(g)(y,x)))
				       (range(1,w,[]))))
  in
      print "\n"
  end;

fun displayGrid(g:grid, w, h) =
  map (displayLine(g,w))
      (range(1,h,[]))

fun gridIt(w,h) =
  let val xs = range(1,(w+1),[])
      val ys = range(1,(h+1),[]) in
      concat(List.map (fn x => (List.map (fn y => (x,y))
					 ys))
		      xs)
  end;

fun randomGrid((shrgx,congy),p,w,h) =
  let val rng = Random.rand(congy,shrgx)
      val xs = range(1,(w+1),[])
      val ys = range(1,(h+1),[]) in
      map (coordToRandomState(rng,p))
	  (gridIt(w,h))
  end;

fun game(iterations, w, h) =
  let val r = (10,10)
      val g = (foldl (fn (_,s) => tick(s))
		     (randomGrid(r,0.3,w,h))
		     (range(1,iterations,[])))
  in
     displayGrid(g,w,h)
  end;

fun main(program_name, args) =
  let val n = 21
      val w = 30
      val h = 30
      val _ = game(n,w,h)
  in
      1
  end;

end;
