import Base.==
import Base.show
import Base.print
import Base.string
import Base.isempty

"""
To do:
 - 4 'moves' concrete types: move, take, castle, promote. 4 apply! functions; 4 takeback! functions
 - pawn promotion
 - no castling across check
 - 3-in-a-row rule...
 - incheck sees if king could take knight with a knitesmove, etc..
 - use a hash table - https://en.wikipedia.org/wiki/Hash_table - storing board evaluations at a given depth.
 - Serialize board. Hash result. Create a hash table mapping from hash start to [white_in_check, black_in_check]
 - moves = line + tree
 - currentmove = length(moves) + 1
 - only check right castle if e1:f1 and h1:g1 in moves.
 - only check left castle if e1:d1 and a1:b1 and a1:c1 in moves.
 - get parallel threads running + computer thinking while human is thinking.
 - each evalutation (for each starting point and ply) gets put into the massive hash table.
 - iterative deepening
"""

abstract type moves end

type m0ve <: moves
  oldsq::Int8
  newsq::Int8
  piece::Int8
end
type take <: moves
  oldsq::Int8
  newsq::Int8
  piece::Int8
  takes::Int8
end
type castle <: moves
  oldsq1::Int8
  newsq1::Int8
  oldpc2::Int8
  newpc2::Int8
end
type promote <: moves
  oldsq::Int8
  newsq::Int8
  oldpc::Int8
  newpc::Int8
  takes::Int8
end

type move <: moves
  oldsq::Int8
  newsq::Int8
  oldpc::Int8
  newpc::Int8
  takes::Int8
end
type extra <: moves
  oldsq::Int8
  newsq::Int8
  oldpc::Int8
  newpc::Int8
  takes::Int8
end

type board
  squares::Array{Int8,1}
  pieces::Array{Int8,1}
  whitesmove::Bool
  moves::Array{moves,1}
end

function ==(m1::moves,m2::moves)
  ((typeof(m1)==typeof(m2)) && 
   (m1.oldsq==m2.oldsq) && 
   (m1.newsq==m2.newsq) && 
   (m1.oldpc==m2.oldpc) && 
   (m1.newpc==m2.newpc) && 
   (m1.takes==m2.takes))
end

LEVEL = 4
VERBOSE = false
NOCATCH = false
NOSQ = 65
ALLOWCASTLING = true
CASTLINGMOVES =     [extra(05,03,05,05,NOSQ), extra(05,07,05,05,NOSQ), extra(61,59,61,61,NOSQ), extra(61,63,61,61,NOSQ)]
CASTLINGTAKEBACKS = [extra(03,05,05,05,NOSQ), extra(07,05,05,05,NOSQ), extra(59,61,61,61,NOSQ), extra(63,61,61,61,NOSQ)]

whitepawn = [09,10,11,12,13,14,15,16]
blackpawn = [49,50,51,52,53,54,55,56]
whiterook = [01,08]
blackrook = [57,64]
whiteknight = [02,07]
blackknight = [58,63]
whitebishop = [03,06]
blackbishop = [59,62]
whitequeen = [04]
blackqueen = [60]
whiteking = [05]
blackking = [61]

print(io::IO, b::board) = show(io, b)
show(io::IO, b::board) = print(io, b)

function newboard()
  squares = [collect(1:16); NOSQ*ones(Int32,32); collect(49:64)]
  pieces = [collect(1:16); zeros(Int,32); collect(49:64); 0]
  whitesmove = true
  moves = Array{move,1}()
  board(squares,pieces,whitesmove,moves)
end

function apply!(b::board, m::move)
  b.pieces[m.oldpc]=NOSQ
  b.pieces[m.newpc]=m.newsq
  b.squares[m.oldsq]=NOSQ
  b.squares[m.newsq]=m.newpc
  b.pieces[m.takes]=NOSQ
  b.whitesmove=!b.whitesmove
  push!(b.moves,m)
  return
end

function apply!(b::board, m::m0ve)
  b.squares[m.oldsq]=NOSQ
  b.pieces[m.piece]=m.newsq
  b.squares[m.newsq]=m.piece
  b.whitesmove=!b.whitesmove
  push!(b.moves,m)
  return
end

function apply!(b::board, m::take)
  b.squares[m.oldsq]=NOSQ
  b.pieces[m.piece]=m.newsq
  b.pieces[m.takes]=NOSQ
  b.squares[m.newsq]=m.piece
  b.whitesmove=!b.whitesmove
  push!(b.moves,m)
  return
end

function apply!(b::board, m::castle)
  b.squares[m.oldsq1] = NOSQ
  b.pieces[m.piece1] = m.newsq1
  b.squares[m.newsq1] = m.piece1
  b.squares[m.oldsq2] = NOSQ
  b.pieces[m.piece2] = m.newsq2
  b.squares[m.newsq2] = m.piece2
  b.whitesmove=!b.whitesmove
  push!(b.moves,m)
  return
end

function apply!(b::board, m::promote)
  b.squares[m.oldsq]=NOSQ
  b.pieces[m.oldpc]=NOSQ
  b.pieces[m.newsq]=m.newpc
  b.pieces[m.newpc]=m.newsq
  b.pieces[m.takes]=NOSQ
  b.whitesmove=!b.whitesmove
  push!(b.moves,m)
  return
end

function apply!(b::board, m1::extra)
  if m1 in CASTLINGMOVES
    function getm2(m1)
        if m1.oldsq==05 && m1.newsq==03        # QUEENSIDE CASTLE
            return extra(01,04,01,01,NOSQ)
        elseif m1.oldsq==05 && m1.newsq==07
            return extra(08,06,08,08,NOSQ)
        elseif m1.oldsq==61 && m1.newsq==59
            return extra(56,60,56,56,NOSQ)
        elseif m1.oldsq==61 && m1.newsq==63
            return extra(64,62,64,64,NOSQ)
        elseif m1.oldsq==03 && m1.newsq==05    # TAKE BACK CASTLE
            return extra(04,01,01,01,NOSQ)
        elseif m1.oldsq==07 && m1.newsq==05
            return extra(06,08,08,08,NOSQ)
        elseif m1.oldsq==59 && m1.newsq==61
            return extra(60,56,60,56,NOSQ)
        elseif m1.oldsq==63 && m1.newsq==61
            return extra(62,64,62,64,NOSQ)
        end
    end
    m2 = getm2(m1)
    b.pieces[m1.oldpc]=NOSQ
    b.pieces[m1.newpc]=m1.newsq
    b.squares[m1.oldsq]=NOSQ
    b.squares[m1.newsq]=m1.newpc
    b.pieces[m2.oldpc]=NOSQ
    b.pieces[m2.newpc]=m2.newsq
    b.squares[m2.newsq]=m2.newpc
    b.squares[m2.oldsq]=NOSQ
    push!(b.moves, m1)
    b.whitesmove = !b.whitesmove
  elseif m1 in CASTLINGTAKEBACKS
    function gm2(m1)
        if m1.oldsq==03 && m1.newsq==05        # QUEENSIDE CASTLE
            return extra(04,01,01,01,NOSQ)
        elseif m1.oldsq==07 && m1.newsq==05
            return extra(06,08,08,08,NOSQ)
        elseif m1.oldsq==59 && m1.newsq==61
            return extra(60,56,56,56,NOSQ)
        elseif m1.oldsq==63 && m1.newsq==61
            return extra(62,64,64,64,NOSQ)
        elseif m1.oldsq==05 && m1.newsq==03    # TAKE BACK CASTLE
            return extra(01,04,01,01,NOSQ)
        elseif m1.oldsq==05 && m1.newsq==07
            return extra(08,06,08,08,NOSQ)
        elseif m1.oldsq==61 && m1.newsq==59
            return extra(56,60,60,56,NOSQ)
        elseif m1.oldsq==61 && m1.newsq==63
            return extra(64,62,62,64,NOSQ)
        end
    end
    m2 = gm2(m1)
    b.pieces[m1.oldpc]=NOSQ
    b.pieces[m1.newpc]=m1.newsq
    b.squares[m1.oldsq]=NOSQ
    b.squares[m1.newsq]=m1.newpc
    b.pieces[m2.oldpc]=NOSQ
    b.pieces[m2.newpc]=m2.newsq
    b.squares[m2.newsq]=m2.newpc
    b.squares[m2.oldsq]=NOSQ
    push!(b.moves, m1)
    b.whitesmove = !b.whitesmove
  else
    b.pieces[m1.oldpc]=NOSQ
    b.pieces[m1.newpc]=m1.newsq
    b.squares[m1.oldsq]=NOSQ
    b.squares[m1.newsq]=m1.newpc
    push!(b.moves, m1)
    b.whitesmove = !b.whitesmove
  end
end

function value(b)
  values = [5;3;3;9;1000;3;3;5;ones(Int32,8);zeros(Int,32);-ones(Int32,8);-5;-3;-3;-9;-1000;-3;-3;-5;0]
  return sum(values[b.pieces .!= NOSQ])
end

up = (x) -> begin
  (x<1||x>64) && return 0
  x+8>65 && return 0
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
  x+8>65 && return 0
  x+7
end
upRight = (x) -> begin
  (x<1||x>64) && return 0
  x+8>65 && return 0
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

pawnUnmoved = (p,b) -> ((9<=p<=16 || 49<=p<=56) && b.pieces[p]==p)

pieces = Dict(1=>"wR",2=>"wN",3=>"wB",4=>"wQ",5=>"wK",6=>"wB",7=>"wN",8=>"wR",
              9=>"wP",10=>"wP",11=>"wP",12=>"wP",13=>"wP",14=>"wP",15=>"wP",16=>"wP",
             49=>"bP",50=>"bP",51=>"bP",52=>"bP",53=>"bP",54=>"bP",55=>"bP",56=>"bP",
             57=>"bR",58=>"bN",59=>"bB",60=>"bQ",61=>"bK",62=>"bB",63=>"bN",64=>"bR",
             NOSQ=>"  ",0=>"  ")
row(x) = div((x-1)%64,8)+1
col(x) = mod((x-1)%64,8)+1
isempty(x, board) = 0<x<65 && board.squares[x]==NOSQ
iswhite(x, board) = 0<x<65 && 0<board.squares[x]<17
isblack(x, board) = 0<x<65 && 48<board.squares[x]<NOSQ

# string manipulations down here
colstr(x) = ["A","B","C","D","E","F","G","H"][col(x)]
colnum(x) = parse(Int, x) - 9
rowstr(x) = string(row(x))
square(x) = string(colstr(x),rowstr(x))
isopposite(whitesmove, x, b) = ifelse(whitesmove, isblack(x, b), iswhite(x, b))
isblack(piece) = piece[1]=='b'
iswhite(piece) = piece[1]=='w'

function tomove(b::board; extratype=false)
  function _tomove(m)
    fromcol,fromrow,tocol,torow = colnum(m[1]),parse(Int,m[2]),colnum(m[4]),parse(Int,m[5])
    from = fromcol + 8*(fromrow-1)
    to = tocol + 8*(torow-1)
    piece=b.squares[from]
    if extratype
        newpc = piece
        if length(m)==6
            newpcstr = m[6]
            newpc = ifelse(b.whitesmove,
                           ifelse(newpcstr=="N",piece+16,piece+8),
                           ifelse(newpcstr=="N",piece-16,piece-8))
        end
        return extra(from,to,piece,newpc,b.squares[to])
    else
        return move(from,to,piece,piece,b.squares[to])
    end
  end
end

function pawnMoves(b::board; whitesmove=b.whitesmove)
  mymoves = moves[]
  inc = ifelse(whitesmove, up, down)
  pieces=ifelse(whitesmove, whitepawn, blackpawn) 
  for piece in pieces
    from = b.pieces[piece]
    to = inc(from)
    if isempty(to,b)
      if (whitesmove && row(to)==8)
        push!(mymoves,extra(from,to,piece,piece+08,NOSQ))
        push!(mymoves,extra(from,to,piece,piece+16,NOSQ))
      elseif (!whitesmove && row(to)==1)
        push!(mymoves,extra(from,to,piece,piece-08,NOSQ))
        push!(mymoves,extra(from,to,piece,piece-16,NOSQ))
      else
        push!(mymoves,move(from,to,piece,piece,NOSQ))
      end
      to = inc(to)
      if pawnUnmoved(piece,b) && isempty(to,b)
        push!(mymoves,move(from,to,piece,piece,NOSQ))
      end
    end
    for to in [inc(left(from)), inc(right(from))]
      if isopposite(whitesmove,to,b)
        if (whitesmove && row(to)==8)
          push!(mymoves,move(from,to,piece,piece+08,b.squares[to]))
          push!(mymoves,move(from,to,piece,piece+16,b.squares[to]))
        elseif (!whitesmove && row(to)==1)
          push!(mymoves,move(from,to,piece,piece+08,b.squares[to]))
          push!(mymoves,move(from,to,piece,piece+16,b.squares[to]))
        else
          push!(mymoves,move(from,to,piece,piece,b.squares[to]))
        end
      end
    end
  end
  mymoves
end

function knightMoves(b::board; whitesmove=b.whitesmove)
  mymoves = move[]
  pieces = ifelse(whitesmove, whiteknight, blackknight)
  for piece in pieces
    from = b.pieces[piece]
    for m in [upUpLeft upUpRight upLeftLeft upRightRight downLeftLeft downRightRight downDownLeft downDownRight]
      to = m(from)
      if isempty(to,b)
        push!(mymoves,move(from,to,piece,piece,NOSQ))
      elseif isopposite(whitesmove,to,b)
        push!(mymoves,move(from,to,piece,piece,b.squares[to]))
      end
    end
  end
  mymoves
end

function crossboard(b::board, pieces, whitesmove, increments, multistep)
  mymoves = move[]
  for piece in pieces
    from = b.pieces[piece]
    for inc in increments
      to = from
      while true
        to = inc(to)
        if isempty(to,b)
          push!(mymoves, move(from,to,piece,piece,NOSQ))
          if !multistep
            break
          end
        elseif isopposite(whitesmove,to,b)
          push!(mymoves, move(from,to,piece,piece,b.squares[to]))
          break
        else
          break
        end
      end
    end
  end
  return mymoves
end

function rookMoves(b::board; whitesmove=b.whitesmove)
    pieces = ifelse(whitesmove, whiterook, blackrook)
    increments = [up down left right]
    crossboard(b, pieces, whitesmove, increments, true)
end

function bishopMoves(b::board; whitesmove=b.whitesmove)
    pieces = ifelse(whitesmove, whitebishop, blackbishop)
    increments = [upLeft upRight downLeft downRight]
    crossboard(b, pieces, whitesmove, increments, true)
end

function queenMoves(b::board; whitesmove=b.whitesmove)
    pieces = ifelse(whitesmove, whitequeen, blackqueen)
    increments = [up down left right upLeft upRight downLeft downRight]
    crossboard(b, pieces, whitesmove, increments, true)
end

function kingMoves(b::board; whitesmove=b.whitesmove)
    pieces = ifelse(whitesmove, whiteking, blackking)
    increments = [up down left right upLeft upRight downLeft downRight]
    crossboard(b, pieces, whitesmove, increments, false)
end

function castlingMoves(b::board; whitesmove=b.whitesmove)
    mymoves = moves[]
    pieces = ifelse(whitesmove, whiteking, blackking)
    unmoved(piece) = !(piece in [m.oldpc for m in b.moves])
    if whitesmove
        if unmoved(1) && unmoved(5) && isempty(4,b) && isempty(3,b) && isempty(2,b)
            push!(mymoves, extra(5,3,5,5,NOSQ))
        end
        if unmoved(8) && unmoved(5) && isempty(6,b) && isempty(7,b)
            push!(mymoves, extra(5,7,5,5,NOSQ))
        end
    else
        if unmoved(57) && unmoved(61) && isempty(60,b) && isempty(59,b) && isempty(58,b)
            push!(mymoves, extra(61,59,61,61,NOSQ))
        end
        if unmoved(64) && unmoved(61) && isempty(62,b) && isempty(63,b)
            push!(mymoves, extra(61,63,61,61,NOSQ))
        end
    end
    return mymoves
end

function possiblemoves(b::board)
    possible = [pawnMoves(b); rookMoves(b); knightMoves(b); 
                bishopMoves(b); queenMoves(b); kingMoves(b)]
    if ALLOWCASTLING
        possible = [possible; castlingMoves(b)]
    end
    return possible
end

function takeback!(b::board, m::moves)
  undo = ifelse(typeof(m)==move, move(m.newsq,m.oldsq,m.newpc,m.oldpc,NOSQ), extra(m.newsq,m.oldsq,m.newpc,m.oldpc,NOSQ)) 
  taken = m.takes
  apply!(b, undo)
  pop!(b.moves)
  if taken != NOSQ
    b.pieces[taken] = undo.oldsq
    b.squares[undo.oldsq] = taken
  end
end

function takeback!(b::board, m::m0ve)
  undo = m0ve(m.newsq,m.oldsq,m.piece) 
  apply!(b, undo)
  pop!(b.moves)
end

function takeback!(b::board, m::take)
  undo = move(m.newsq,m.oldsq,m.piece,NOSQ)
  taken = m.takes
  apply!(b, undo)
  pop!(b.moves)
  b.pieces[taken] = undo.oldsq
  b.squares[undo.oldsq] = taken
end

function takeback!(b::board, m::castle)
  undo = castle(m.newsq1, m.oldsq1, m.newpc2, m.oldpc2)
  apply!(b, undo)
  pop!(b.moves)
end

function takeback!(b::board, m::promote)
  undo = move(m.newsq,m.oldsq,m.newpc,m.oldpc,NOSQ)
  taken = m.takes
  apply!(b, undo)
  pop!(b.moves)
  if taken != NOSQ
    b.pieces[taken] = undo.oldsq
    b.squares[undo.oldsq] = taken
  end
end

function takeback!(b::board)
  m = pop!(b.moves)
  takeback!(b, m)
end

function incheck(b::board, white)
    whitesmove = b.whitesmove
    k = ifelse(white, 5, 61)
    b.whitesmove = ifelse(white, false, true)
    for m in possiblemoves(b)
        if m.newsq == b.pieces[k]
            b.whitesmove = whitesmove
            return true
        end
    end
    b.whitesmove = whitesmove
    return false
end

function intocheck(b::board, m::move)
    apply!(b, m)
    result = incheck(b, !b.whitesmove)
    takeback!(b)
    return result
end

function intocheck(b::board, m::extra)
    #apply!(b, m)
    #result = incheck(b, !b.whitesmove)
    #takeback!(b)
    return false
end

function allowedmoves(b::board)
    [m for m in possiblemoves(b) if !(intocheck(b, m))]
end 

function minimax(b::board, depth; toconsider=moves[])
  if length(toconsider)==0
    toconsider = shuffle(possiblemoves(b))
  end
  if depth == 0 || length(moves) == 0
    score = ifelse(b.whitesmove, 1, -1) * value(b)
    return move(0, 0, NOSQ, NOSQ), score
  end
  bestmove, bestscore = move(0, 0, NOSQ, NOSQ), -Inf
  for m in toconsider
    apply!(b, m)
    s = -minimax(b, depth-1)[2]
    if VERBOSE
      println(depth," :  ","    "^(3-depth),show(m), " : ", s)
    end
    if s > bestscore
      bestmove, bestscore = m, s
    end
    takeback!(b)
  end
  return bestmove, bestscore
end

function alphabeta(bi::board, depth, α, β, whitesmove; toconsider=moves[])
  if length(toconsider)==0
    toconsider = shuffle(possiblemoves(bi))
  end
  if depth == 0
    return move(0, 0, 0, 0, NOSQ), value(bi)
  end
  if whitesmove
    mb, v = move(0, 0, 0, 0, NOSQ), -Inf
    for mi in toconsider
      apply!(bi, mi)
      assert(bi.whitesmove==false)
      mr, s = alphabeta(bi,depth-1,α,β,false)
      takeback!(bi)
      if s > v
        v, mb = s, mi
      end
      α = max(α, v)
      if β <= α
        break
      end
    end
    if VERBOSE
      try
        println("HERE 1 ",depth," :  ","    "^(3-depth),show(mb), " : ", v, " α=$α β=$β")
      catch
      end
    end
    return mb, v
  else
    mb, v = move(0, 0, 0, 0, NOSQ), +Inf
    for mi in toconsider
      apply!(bi, mi)
      assert(bi.whitesmove==true)
      mr, s = alphabeta(bi,depth-1,α,β,true)
      takeback!(bi)
      if s < v
        v, mb = s, mi
      end
      β = min(β, v)
      if β <= α
        break
      end
    end
    if VERBOSE
      try
        println("HERE 2 ",depth," :  ","    "^(3-depth),show(mb), " : ", v, " α=$α β=$β")
      catch
      end
    end
    return mb, v
  end
end

function input(prompt::AbstractString="")
  print(prompt)
  chomp(readline())
end

function showmoves(b::board)
    toshow = map(show,allowedmoves(b))
    join(toshow,", ")
end

function prnt(piece)
  if iswhite(piece)
    print_with_color(:blue, "$piece ")
  else
    print_with_color(:red, "$piece ")
  end
end

function show(b::board)
  taken = find((x)->x==NOSQ,b.pieces[1:64])
  blackTaken = [pieces[x] for x in taken[taken.>16]]
  whiteTaken = [pieces[x] for x in taken[taken.<17]]
  blackPrisoners = join(whiteTaken,", ")
  whitePrisoners = join(blackTaken,", ")
  print_with_color(:red,"\n   =========================\n")
  for row in 8:-1:1
    print_with_color(:blue, "$row | ")
    for col in 1:8
      #println(row," ",col," ",col+(row-1)*8," ",b.squares[col+(row-1)*8])
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
  print(ifelse(b.whitesmove, "\nWhite to move. ", "\nBlack to move. "))
end

function show(io::IO, b::board)
  show(b)
  print("$(length(allowedmoves(b))) available moves: \n  $(showmoves(b))\n")
end

show(m::moves) = string(square(m.oldsq),ifelse(m.takes!=NOSQ,"x",":"),square(m.newsq))

function checkmate(b)
    winner = ifelse(!b.whitesmove,"WHITE","BLACK")
    return "$winner WINS!"
end

function play(b; autoplay=false)
  while true
    b2 = deepcopy(b)
    allowed = allowedmoves(b)
    #try
      if !autoplay && b.whitesmove
        print(b)
        if length(allowed) == 0
           return checkmate(b)
        end
        mstr = input("\n> ")
        assert(mstr in map(show, allowed))
        castling = mstr in map(show,CASTLINGMOVES)
        m = ifelse(castling,tomove(b;extratype=true)(mstr),tomove(b)(mstr))
        apply!(b, m)
      end
      print(b)
      b2 = deepcopy(b)
      allowed = allowedmoves(b2)
      tic = time()
      if length(allowed) == 0
         return checkmate(b2)
      end
      #m, s = minimax(b2, LEVEL; toconsider=shuffle(allowed))
      m, s = alphabeta(b2, LEVEL, -Inf, Inf, b2.whitesmove; toconsider=shuffle(allowed))
      toc = time()
      elapsed = Base.Dates.Second(round(toc-tic))
      print("\n> ",show(m)," elapsed time: $elapsed\n")
      sleep(1)
      apply!(b,m)
    #catch e
    #  if NOCATCH
    #    throw(e)
    #  end
    #  print(b)
    #  if isa(e, InterruptException)
    #    println("Breaking out of game")
    #    break
    #  end
    #  println("Incorrect move. Try again...")
    #end
  end
end

b = newboard()
play(b, autoplay=false)
