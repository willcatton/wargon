import Base.==
import Base.show
import Base.print
import Base.string
import Base.isempty

"""
To do:
 - 3-in-a-row rule...
 - incheck sees if king could take knight with a knitesmove, etc..
 - use a hash table - https://en.wikipedia.org/wiki/Hash_table - storing board evaluations at a given depth.
 - Serialize board. Hash result. Create a hash table mapping from hash start to [white_in_check, black_in_check]
 - only check right castle if e1:f1 and h1:f1 in moves.
 - only check left castle if e1:d1 and a1:b1 and a1:c1 in moves.
 - get parallel threads running + computer thinking while human is thinking.
 - each evalutation (for each starting point and ply) gets put into the massive hash table.
 - iterative deepening
"""

abstract type moves end

type move <: moves
  oldsq::Int8
  newsq::Int8
  oldpc::Int8
  newpc::Int8
  takes::Int8
end
type castle <: moves
  oldsq::Int8
  newsq::Int8
  oldpc::Int8
  oldsq2::Int8
  newsq2::Int8
  oldpc2::Int8
end
type promote <: moves
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
function apply!(b::board, m::castle)
  b.squares[m.oldsq] = NOSQ
  b.pieces[m.oldpc] = m.newsq
  b.squares[m.newsq] = m.oldpc
  b.squares[m.oldsq2] = NOSQ
  b.pieces[m.oldpc2] = m.newsq2
  b.squares[m.newsq2] = m.oldpc2
  b.whitesmove=!b.whitesmove
  push!(b.moves,m)
  return
end
function apply!(b::board, m::promote)
  b.pieces[m.oldpc]=NOSQ
  b.pieces[m.newpc]=m.newsq
  b.squares[m.oldsq]=NOSQ
  b.squares[m.newsq]=m.newpc
  b.pieces[m.takes]=NOSQ
  b.whitesmove=!b.whitesmove
  push!(b.moves,m)
  return
end
function takeback!(b::board, m::move)
  undo = move(m.newsq,m.oldsq,m.newpc,m.oldpc,NOSQ)
  taken = m.takes
  apply!(b, undo)
  pop!(b.moves)
  if taken != NOSQ
    b.pieces[taken] = undo.oldsq
    b.squares[undo.oldsq] = taken
  end
end
function takeback!(b::board, m::castle)
  undo = castle(m.newsq, m.oldsq, m.oldpc, m.newsq2, m.oldsq2, m.oldpc2)
  apply!(b, undo)
  pop!(b.moves)
  return
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

LEVEL = 4
VERBOSE = false
NOCATCH = false
NOSQ = 65
whitepawn = [09,10,11,12,13,14,15,16]
blackpawn = [49,50,51,52,53,54,55,56]
whiterook = [01,08]
blackrook = [57,64]
whiteknight = [02,07,25,26,27,28,29,30,31,32]
blackknight = [58,63,33,34,35,36,37,38,39,40]
whitebishop = [03,06]
blackbishop = [59,62]
whitequeen = [04,17,18,19,20,21,22,23,24]
blackqueen = [60,41,42,43,44,45,46,47,48]
whiteking = [05]
blackking = [61]
castlingmoves = [castle(05,03,05,01,04,01), castle(05,07,05,08,06,08), castle(61,59,61,57,60,57), castle(61,63,61,64,62,64)]

print(io::IO, b::board) = show(io, b)
show(io::IO, b::board) = print(io, b)

function newboard()
  squares = [collect(1:16); NOSQ*ones(Int32,32); collect(49:64)]
  pieces = [collect(1:16); zeros(Int,32); collect(49:64); 0]
  whitesmove = true
  moves = Array{move,1}()
  board(squares,pieces,whitesmove,moves)
end

function value(b)
  values = [+5;+3;+3;+9;+1000;+3;+3;+5;
            +1*ones(Int,8);
            +9*ones(Int,8);
            +3*ones(Int,8);
            -3*ones(Int,8);
            -9*ones(Int,8);
            -ones(Int32,8);
            -5;-3;-3;-9;-1000;-3;-3;-5;
            0]
  return sum(values[b.pieces .!= NOSQ])
end

up(x) = ifelse(x<1||x>57,0,x+8)
down(x) = ifelse(x<9||x>64,0,x-8)
left(x) = ifelse(x<1||x>64||mod(x-1,8)==0,0,x-1)
right(x) = ifelse(x<1||x>64||mod(x+1,8)==1,0,x+1)
upLeft(x) = ifelse(x<1||x>57||mod(x-1,8)==0,0,x+7)
upRight(x) = ifelse(x<1||x>57||mod(x+1,8)==1,0,x+9)
downLeft(x) = ifelse(x<10||x>64||mod(x-1,8)==0,0,x-9)
downRight(x) = ifelse(x<8||x>64||mod(x+1,8)==1,0,x-7)
upUpLeft(x) = ifelse(mod(x+15,8)==0||x>49||x<1,0,x+15)
upUpRight(x) = ifelse(mod(x+17,8)==1||x>47||x<1,0,x+17)
upLeftLeft(x) = ifelse(mod(x+6,8) in [0,7]||x>58||x<1,0,x+6)
upRightRight(x) = ifelse(mod(x+10,8) in [1,2]||x>54||x<1,0,x+10)
downLeftLeft(x) = ifelse(mod(x-10,8) in [0,7]||x<11||x>64,0,x-10)
downRightRight(x) = ifelse(mod(x-6,8) in [1,2]||x<7||x>64,0,x-6)
downDownLeft(x) = ifelse(mod(x-17,8)==0||x<18||x>64,0,x-17)
downDownRight(x) = ifelse(mod(x-15,8)==1||x<16||x>64,0,x-15)

pawnUnmoved(p,b) = ((9<=p<=16 || 49<=p<=56) && b.pieces[p]==p)

row(x) = div((x-1)%64,8)+1
col(x) = mod((x-1)%64,8)+1
isempty(x, board) = 0<x<65 && board.squares[x]==NOSQ
iswhite(x, board) = 0<x<65 && 0<board.squares[x]<17
isblack(x, board) = 0<x<65 && 48<board.squares[x]<NOSQ

# string manipulations down here
pieces = ["wR","wN","wB","wQ","wK","wB","wN","wR",
          "wP","wP","wP","wP","wP","wP","wP","wP",
          "wQ","wQ","wQ","wQ","wQ","wQ","wQ","wQ",
          "wN","wN","wN","wN","wN","wN","wN","wN",
          "bN","bN","bN","bN","bN","bN","bN","bN",
          "bQ","bQ","bQ","bQ","bQ","bQ","bQ","bQ",
          "bP","bP","bP","bP","bP","bP","bP","bP",
          "bR","bN","bB","bQ","bK","bB","bN","bR",
          "  "]
colstr(x) = ["A","B","C","D","E","F","G","H"][col(x)]
colnum(x) = parse(Int, x) - 9
rowstr(x) = string(row(x))
square(x) = string(colstr(x),rowstr(x))
isopposite(whitesmove, x, b) = ifelse(whitesmove, isblack(x, b), iswhite(x, b))
isblack(piece) = piece[1]=='b'
iswhite(piece) = piece[1]=='w'

function tomove(b::board)
  function _tomove(m)
    fromcol,fromrow,tocol,torow = colnum(m[1]),parse(Int,m[2]),colnum(m[4]),parse(Int,m[5])
    from = fromcol + 8*(fromrow-1)
    to = tocol + 8*(torow-1)
    piece=b.squares[from]
    if piece in [whiteking; blackking]
        if abs(from-to)==2
            return first([x for x in castlingmoves if x.oldsq==from && x.newsq==to])
        end
    end 
    if length(m)==7 
        newpcstr = m[7]
        if (piece in whitepawn) && (row(to)==8)
            newpc = ifelse(newpcstr=='N',piece+16,piece+8)
            return promote(from,to,piece,newpc,b.squares[to]) 
        elseif (piece in blackpawn) && (row(to)==1)
            newpc = ifelse(newpcstr=='N',piece-16,piece-8)
            return promote(from,to,piece,newpc,b.squares[to])
        end
    end
    return move(from,to,piece,piece,b.squares[to])
  end
end

function pawnMoves(b::board; whitesmove=b.whitesmove)
  mymoves = moves[]
  inc = ifelse(whitesmove, up, down)
  pieces = ifelse(whitesmove, whitepawn, blackpawn) 
  for piece in pieces
    from = b.pieces[piece]
    if from != NOSQ
      to = inc(from)
      if isempty(to,b)
        if (whitesmove && row(to)==8)
          push!(mymoves,promote(from,to,piece,piece+08,NOSQ))
          push!(mymoves,promote(from,to,piece,piece+16,NOSQ))
        elseif (!whitesmove && row(to)==1)
          push!(mymoves,promote(from,to,piece,piece-08,NOSQ))
          push!(mymoves,promote(from,to,piece,piece-16,NOSQ))
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
  end
  mymoves
end

function knightMoves(b::board; whitesmove=b.whitesmove)
  mymoves = move[]
  pieces = ifelse(whitesmove, whiteknight, blackknight)
  for piece in pieces
    from = b.pieces[piece]
    if from != NOSQ
      for m in [upUpLeft upUpRight upLeftLeft upRightRight downLeftLeft downRightRight downDownLeft downDownRight]
        to = m(from)
        if isempty(to,b)
          push!(mymoves,move(from,to,piece,piece,NOSQ))
        elseif isopposite(whitesmove,to,b)
          push!(mymoves,move(from,to,piece,piece,b.squares[to]))
        end
      end
    end
  end
  mymoves
end

function crossboard(b::board, pieces, whitesmove, increments, multistep)
  mymoves = move[]
  for piece in pieces
    from = b.pieces[piece]
    if from != NOSQ
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

function castlingMoves(b::board, possible::Array{moves,1}; whitesmove=b.whitesmove)
    mymoves = moves[]
    pieces = ifelse(whitesmove, whiteking, blackking)
    unmoved(piece) = !(piece in [m.oldpc for m in b.moves])
    if whitesmove
        if (move(05,04,05,05,NOSQ) in possible) && (move(01,04,01,01,NOSQ) in possible) && unmoved(01) && unmoved(05)
            push!(mymoves, castle(05,03,05,01,04,01))
        end
        if (move(05,06,05,05,NOSQ) in possible) && (move(08,06,08,08,NOSQ) in possible) && unmoved(05) && unmoved(08)
            push!(mymoves, castle(05,07,05,08,06,08))
        end
    else
        if (move(61,60,61,61,NOSQ) in possible) && (move(57,60,57,57,NOSQ) in possible) && unmoved(57) && unmoved(61)
            push!(mymoves, castle(61,59,61,57,60,57))
        end
        if (move(61,62,61,61,NOSQ) in possible) && (move(64,62,64,64,NOSQ) in possible) && unmoved(64) && unmoved(61)
            push!(mymoves, castle(61,63,61,64,62,64))
        end
    end
    return mymoves
end

function possiblemoves(b::board)
    possible = [pawnMoves(b); rookMoves(b); knightMoves(b); 
                bishopMoves(b); queenMoves(b); kingMoves(b)]
    possible = [possible; castlingMoves(b,possible)]
    return possible
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

function intocheck(b::board, m::moves)
    apply!(b, m)
    result = incheck(b, !b.whitesmove)
    takeback!(b)
    return result
end

function intocheck(b::board, m::castle)
    #apply!(b, m)
    #result = incheck(b, !b.whitesmove)
    #takeback!(b)
    return false
end

function allowedmoves(b::board)
    [m for m in possiblemoves(b) if !(intocheck(b, m))]
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
  blackTaken = [pieces[x] for x in taken[taken.>32]]
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
  print(ifelse(b.whitesmove, "\nWhite to move. ", "\nBlack to move. "))
end

function show(io::IO, b::board)
  show(b)
  print("$(length(allowedmoves(b))) available moves: \n  $(showmoves(b))\n")
end

show(m::moves) = string(square(m.oldsq),ifelse(m.takes!=NOSQ,"x",":"),square(m.newsq))
show(m::castle) = string(square(m.oldsq),":",square(m.newsq))
show(m::promote) = string(square(m.oldsq),ifelse(m.takes!=NOSQ,"x",":"),square(m.newsq),":",ifelse(25<=m.newpc<=42,"N","Q"))

function checkmate(b)
    winner = ifelse(!b.whitesmove,"WHITE","BLACK")
    return "$winner WINS!"
end

function play(b; autoplay=false)
  while true
    b2 = deepcopy(b)
    allowed = allowedmoves(b)
    try
      if !autoplay && b.whitesmove
        print(b)
        if length(allowed) == 0
           return checkmate(b)
        end
        mstr = input("\n> ")
        assert(mstr in map(show, allowed))
        m = tomove(b)(mstr)
        apply!(b, m)
      end
      print(b)
      b2 = deepcopy(b)
      allowed = allowedmoves(b2)
      tic = time()
      if length(allowed) == 0
         return checkmate(b2)
      end
      m, s = alphabeta(b2, LEVEL, -Inf, Inf, b2.whitesmove; toconsider=shuffle(allowed))
      toc = time()
      elapsed = Base.Dates.Second(round(toc-tic))
      print("\n> ",show(m)," elapsed time: $elapsed\n")
      sleep(1)
      apply!(b,m)
    catch e
      if NOCATCH
        throw(e)
      end
      print(b)
      if isa(e, InterruptException)
        println("Breaking out of game")
        break
      end
      println("Incorrect move. Try again...")
    end
  end
end

b = try
   b
catch
   newboard()
end
play(b, autoplay=false)
