import Base.show
import Base.print
import Base.string

"""
To create a chess playing program:

- board (letterbox representation mapping each square to a piece, and each piece to a square). DONE.
- display function. DONE.
- move generator. DONE.
- static evaluator (e.g. a score in mPawn). DONE.
- function which applies a move to the board. DONE.
- recursive function minimax(board, depth) which returns bestmove, bestscore. DONE.
- Different board used for thinking. DONE.
- Recognize check and checkmate. DONE.

To do:
- Represent moves with a quadruple: (from, to, piecetaken, check), where piecetaken=0 if no piece is taken. 
- Serialize board. Hash result. Create a dict from hash start to [white_in_check, black_in_check]
- Use a hash table - https://en.wikipedia.org/wiki/Hash_table - storing board evaluations at a given depth.
- Implement mystyle minimax
- takeback!(b) will pop the latest move and modify the board appropriately. 
- modify show(move) as appropriate.
- disallowcastlerightwhite, disallowcastleftwhite (ints recording move when this occurred)
- currentmove = length(moves) + 1
- only check right castle if e1:f1 and h1:g1 in moves.
- only check left castle if e1:d1 and a1:b1 and a1:c1 in moves.
- Replace minimax with alphabeta
- Pawn promotion.
- 3-in-a-row rule...
- Castling; no castling across check
"""

LEVEL = 2

whitepawn = collect(9:16)
blackpawn = collect(17:24)
whiterook = [1,8]
blackrook = [25,32]
whiteknight = [2,7]
blackknight = [26,31]
whitebishop = [3,6]
blackbishop = [27,30]
whitequeen = [4]
blackqueen = [28]
whiteking = [5]
blackking = [29]

type board
  squares::Array{Int8,1}
  pieces::Array{Int8,1}
  whitesmove::Bool
  movelog::Array{Tuple{Int8,Int8},1}
  moves::Array{Tuple{Int8,Int8,Int8,Bool},1}
  disallowcastling::Array{Bool,1}
end
print(io::IO, b::board) = show(io, b)
show(io::IO, b::board) = print(io, b)
function newboard()
  squares = [collect(1:16); zeros(Int32,32); collect(17:32)]
  pieces = [collect(1:16); collect(49:64)]
  whitesmove = true
  movelog = Array{Tuple{Int8,Int64,String},1}[]
  moves = Array{Tuple{Int8,Int8,Int8,Bool},1}[]
  disallowcastling = [false,false,false,false]
  board(squares,pieces,whitesmove,movelog,moves,disallowcastling)
end
undo = (x) -> (x[2], x[1])
function apply!(b::board, m)
  taken = b.squares[m[2]]
  if 0 < taken <= 32
    b.pieces[taken]=0
  end
  b.squares[m[2]]=b.squares[m[1]]
  b.squares[m[1]]=0
  b.pieces[b.squares[m[2]]]=m[2]
  b.whitesmove = !b.whitesmove
  push!(b.moves, toquad(m, b))
  undo(m), taken
end
function value(b)
  values = [5;3;3;9;1000;3;3;5;ones(Int32,8);-ones(Int32,8);-5;-3;-3;-9;-1000;-3;-3;-5]
  return sum(values[b.pieces .!= 0]) + sum(ifelse.(b.pieces[9:16].>64, 8, 0)) + sum(ifelse.(b.pieces[17:24].>64, -8, 0))
end

up = (x) -> begin
  (x<1||x>64) && return 0
  x+8>64 && return 0
  x+8
end
down = (x) -> begin
  (x<1||x>64) && return 0
  max(x-8,0)
end
left = (x) -> begin
  (x<1||x>64) && return 0
  mod(x-1,8)==0 && return 0
  x-1
end
right = (x) -> begin
  (x<1||x>64) && return 0
  mod(x+1,8)==1 && return 0
  x+1
end
upLeft = (x) -> begin
  (x<1||x>64) && return 0
  mod(x-1,8)==0 && return 0
  x+8>64 && return 0
  x+7
end
upRight = (x) -> begin
  (x<1||x>64) && return 0
  x+8>64 && return 0
  mod(x+1,8)==1 && return 0
  x+9
end
downLeft = (x) -> begin
  (x<1||x>64) && return 0
  mod(x-1,8)==0 && return 0
  max(x-9,0)
end
downRight = (x) -> begin
  (x<1||x>64) && return 0
  mod(x+1,8)==1 && return 0
  max(x-7,0)
end
upUpLeft = (x) -> up(up(left(x)))
upUpRight = (x) -> up(up(right(x)))
upLeftLeft = (x) -> up(left(left(x)))
upRightRight = (x) -> up(right(right(x)))
downLeftLeft = (x) -> down(left(left(x)))
downRightRight = (x) -> down(right(right(x)))
downDownLeft = (x) -> down(down(left(x)))
downDownRight = (x) -> down(down(right(x)))

pawnUnmoved = (p,b) -> (9<=p<=16 && b.pieces[p]==p) || (16<p<25 && b.pieces[p]==p+32)

pieces = Dict(1=>"wR",2=>"wN",3=>"wB",4=>"wQ",5=>"wK",6=>"wB",7=>"wN",8=>"wR",
              9=>"wP",10=>"wP",11=>"wP",12=>"wP",13=>"wP",14=>"wP",15=>"wP",16=>"wP",
             17=>"bP",18=>"bP",19=>"bP",20=>"bP",21=>"bP",22=>"bP",23=>"bP",24=>"bP",
             25=>"bR",26=>"bN",27=>"bB",28=>"bQ",29=>"bK",30=>"bB",31=>"bN",32=>"bR",
              0=>"  ")
row=(x)->div((x-1)%64,8)+1
col=(x)->mod((x-1)%64,8)+1
isEmpty = (x, board) -> 0<x<65 && board.squares[x]==0
isWhite = (x, board) -> 0<x<65 && 0<board.squares[x]<17
isBlack = (x, board) -> 0<x<65 && 16<board.squares[x]<33

# sring manipulations down here
colstr = (x)->["A","B","C","D","E","F","G","H"][col(x)]
colnum = (x)->parse(Int, x) - 9
rowstr = (x)->string(row(x))
square = (x)->string(colstr(x),rowstr(x))
isOpposite(c, x, b) = ifelse(c=="white", isBlack(x, b), isWhite(x, b))
isblack = (piece)->piece[1]=='b'
iswhite = (piece)->piece[1]=='w'

function topair(m::AbstractString)
  fromcol, fromrow, tocol, torow = colnum(m[1]), parse(Int,m[2]), colnum(m[4]), parse(Int,m[5])
  from = fromcol + 8*(fromrow-1)
  to = tocol + 8*(torow-1)
  move = (from, to)
end

function toquad(m::Tuple, b::board)
  color = ifelse(b.whitesmove, "white", "black")
  (from, to) = m
  taken = b.squares[to]
  check = intocheck(b, m, color)
  (from, to, taken, check)
end

#toquad(m::Tuple{Int8,Int64,String}, b::board) = toquad((m[1],m[2]), b)

function pawnMoves(board, color)
  moves = []
  inc = ifelse(color=="white", up, down)
  pieces=ifelse(color=="white", whitepawn, blackpawn) 
  # TODO: color -> whitesmove
  for piece in pieces
    from = board.pieces[piece]
    if 64 < from <=128
      # add moves for queen
    else
      to = inc(from)
      if isEmpty(to,board)
        push!(moves,(from,to,""))
        to = inc(to)
        if pawnUnmoved(piece,board) && isEmpty(to,board)
          push!(moves,(from,to,""))
        end
      end
      for to in [inc(left(from)), inc(right(from))]
        if isOpposite(color,to,board)
          push!(moves,(from,to,"x"))
        end
      end
    end
  end
  moves
end

function knightMoves(board, color)
  moves = []
  pieces = ifelse(color=="white", whiteknight, blackknight)
  for piece in pieces
    from = board.pieces[piece]
    for m in [upUpLeft upUpRight upLeftLeft upRightRight downLeftLeft downRightRight downDownLeft downDownRight]
      to = m(from)
      if isEmpty(to,board)
        push!(moves,(from,to,""))
      elseif isOpposite(color,to,board)
        push!(moves,(from,to,"x"))
      end
    end
  end
  moves
end

function crossboard(board, pieces, color, increments, multistep)
  moves = []
  for piece in pieces
    from = board.pieces[piece]
    for inc in increments
      to = from
      while true
        to = inc(to)
        if isEmpty(to,board)
          push!(moves, (from,to,""))
          if !multistep
            break
          end
        elseif isOpposite(color,to,board)
          push!(moves, (from,to,"x"))
          break
        else
          break
        end
      end
    end
  end
  return moves
end

function rookMoves(board, color)
    pieces = ifelse(color=="white", whiterook, blackrook)
    increments = [up down left right]
    crossboard(board, pieces, color, increments, true)
end

function bishopMoves(board, color)
    pieces = ifelse(color=="white", whitebishop, blackbishop)
    increments = [upLeft upRight downLeft downRight]
    crossboard(board, pieces, color, increments, true)
end

function queenMoves(board, color)
    pieces = ifelse(color=="white", whitequeen, blackqueen)
    increments = [up down left right upLeft upRight downLeft downRight]
    crossboard(board, pieces, color, increments, true)
end

function kingMoves(board, color)
    pieces = ifelse(color=="white", whiteking, blackking)
    increments = [up down left right upLeft upRight downLeft downRight]
    crossboard(board, pieces, color, increments, false)
end

function possiblemoves(b)
  c = ifelse(b.whitesmove, "white", "black")
  moves = [pawnMoves(b,c); rookMoves(b,c); knightMoves(b,c); 
           bishopMoves(b,c); queenMoves(b,c); kingMoves(b,c)]
end

function restore!(board::board, undo, taken)
  quad = pop!(b.moves)
  apply!(board, undo)
  if taken != 0
    board.pieces[taken] = undo[1]
    board.squares[undo[1]] = taken
  end
end

function incheck(b::board, color)
    whitesmove = b.whitesmove
    k = Dict("white"=>5,"black"=>29)[color]
    b.whitesmove = ifelse(color=="white", false, true)
    for m in possiblemoves(b)
        if m[2] == b.pieces[k]
            b.whitesmove = whitesmove
            return true
        end
    end
    b.whitesmove = whitesmove
    return false
end

function intocheck(b::board, m, color)
    println("HERE 1 : $m")
    undoing = apply!(b,m)
    println("HERE 2")
    result = incheck(b, color)
    restore!(b,undoing...)
    return result
end

function allowedmoves(b)
    color = ifelse(b.whitesmove, "white", "black")
    [m for m in possiblemoves(b) if !(intocheck(b, m, color))]
end 

function minimax(board, depth)
  moves = shuffle(allowedmoves(board))
  if depth == 0 || length(moves) == 0
    score = ifelse(board.whitesmove, 1, -1) * value(board)
    #println(depth," :  ","    "^(2-depth),show(move)," : ",score)
    return (0,0,""), score
  end
  best_move, best_score = (0,0,""), -Inf
  for move in moves
    #println("    "^(2-depth),show(move))
    undoing = apply!(board, move)
    #if depth > 1
    #    println(depth," :  ","    "^(3-depth),show(move))
    #end
    score = -minimax(board, depth-1)[2]
    if depth == LEVEL
        # println(show(move)," : ",score)
    end
    if score > best_score
      best_move, best_score = move, score
    end
    restore!(board, undoing...)
  end
  return best_move, best_score
end

function alphabeta(board, depth, α, β, maximizingPlayer)
  moves = shuffle(allowedmoves(board))
  if depth == 0 || length(moves) == 0
    return (0,0,""), value(board)
  end
  if maximizingPlayer
    v, m = -Inf, (0, 0, "")
    for move in moves
      undoing = apply!(board, move)
      s = alphabeta(board,depth-1,α,β,false)[2]
      restore!(board, undoing...)
      if s > v
        v, m = s, move
      end
      α = max(α, v)
      if β <= α
        break
      end
    end
    return m, v
  else
    v, m = +Inf, (0, 0, "")
    for move in moves
      undoing = apply!(board, move)
      s = alphabeta(board,depth-1,α,β,true)[2]
      restore!(board, undoing...)
      if s < v
        v, m = s, move
      end
      α = min(α, v)
      if β <= α
        break
      end
    end
    return m, v
  end
end

function input(prompt::AbstractString="")
  print(prompt)
  chomp(readline())
end

function showmoves(b::board)
    moves = map(show,allowedmoves(b))
    join(moves,", ")
end

function bestmove(b::board, depth)
    m, s = minimax(b, depth)
    show(m)
end

function bestmove!(b::board, depth)
    m = bestmove(b, depth)
    println(m)
    apply!(b, m)
    b
end

bestmove(b::board) = bestmove(b,2)
bestmove!(b::board) = bestmove!(b,2)

function prnt(piece)
  if iswhite(piece)
    print_with_color(:blue, "$piece ")
  else
    print_with_color(:red, "$piece ")
  end
end

function show(board::board)
  taken = find((x)->x==0,b.pieces)
  blackTaken = [pieces[x] for x in taken[taken.>16]]
  whiteTaken = [pieces[x] for x in taken[taken.<17]]
  blackPrisoners = join(whiteTaken,", ")
  whitePrisoners = join(blackTaken,", ")
  print_with_color(:red,"\n   =========================\n")
  for row in 8:-1:1
    print_with_color(:blue, "$row | ")
    for col in 1:8
      piecestr = pieces[b.squares[col+(row-1)*8]]
       if piecestr=="  " 
         if (row+col)%2 == 0
           print(":: ")
         else
           print("   ")
         end
       else
         prnt(piecestr)
       end
    end
    print_with_color(:red, "| ")
    if row==1
      print_with_color(:red, "      ", whitePrisoners)
    elseif row==8
      print_with_color(:blue, "      ", blackPrisoners)
    end
    println("")
  end
  print_with_color(:blue,"   =========================\n")
  print_with_color(:blue,"     A  B  C  D  E  F  G  H  \n")
  print(ifelse(board.whitesmove, "\nWhite to move. ", "\nBlack to move. "))
end

function show(io::IO, b::board)
  show(b)
  print("$(length(allowedmoves(b))) available moves: \n  $(showmoves(b))\n")
end

function show(move::Tuple)
  xorc = (move) -> ifelse(contains(move[3],"x"),"x",":")
  return string(square(move[1]),xorc(move),square(move[2]))
end

function checkmate(b)
    winner = ifelse(!b.whitesmove,"WHITE","BLACK")
    return "$winner WINS!"
end

function play(b; autoplay=false)
  while true
    board = deepcopy(b)
    try
      if !autoplay && b.whitesmove
        print(b)
        allowed = allowedmoves(b)
        if length(allowed) == 0
           return checkmate(b)
        end
        m = input("\n> ")
        assert(m in map(show, allowed))
        apply!(b,topair(m))
        push!(b.movelog, (m[1], m[2]))
      end
      print(b)
      tic = time()
      board = deepcopy(b)
      m, s = minimax(board,LEVEL)
      ########################################
      #m, s = alphabeta(board,LEVEL,-Inf,Inf,false)
      ########################################
      toc = time()
      elapsed = Base.Dates.Second(round(toc-tic))
      if show(m)=="H1:H1"
        return checkmate(b)
      end
      print("\n> ",show(m)," elapsed time: $elapsed\n")
      sleep(1)
      apply!(b,m)
      push!(b.movelog, (m[1],m[2]))
    catch e
      print(board)
      if isa(e, InterruptException)
        println("Breaking out of game")
        break
      end
      throw(e)
      println("Incorrect move. Try again...")
    end
  end
end

#b = newboard()
#play(b, autoplay=true)
