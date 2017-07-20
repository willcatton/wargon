import Base.==
import Base.show
import Base.print
import Base.string
import Base.isempty

LEVEL = 4
ITDEEP = 1
VERBOSE = false
NOCATCH = false
HTSIZE = 100000
AUTOPLAY = false

abstract type moves end

immutable move <: moves
  oldsq::Int
  newsq::Int
  oldpc::Int
  newpc::Int
end
immutable take <: moves
  oldsq::Int
  newsq::Int
  oldpc::Int
  newpc::Int
  takes::Int
end
immutable castle <: moves
  oldsq::Int
  newsq::Int
  oldpc::Int
  oldsq2::Int
  newsq2::Int
  oldpc2::Int
end
immutable promote <: moves
  oldsq::Int
  newsq::Int
  oldpc::Int
  newpc::Int
  takes::Int
end
==(m1::moves,m2::moves) = ((typeof(m1)==typeof(m2)) && (m1.oldsq==m2.oldsq) && (m1.newsq==m2.newsq) &&
                           (m1.oldpc==m2.oldpc) && (m1.newpc==m2.newpc) && (m1.takes==m2.takes))
==(m1::move,m2::move) = ((m1.oldsq==m2.oldsq) && (m1.newsq==m2.newsq) && (m1.oldpc==m2.oldpc) && (m1.newpc==m2.newpc))
==(m1::castle,m2::castle) = ((m1.oldsq==m2.oldsq) && (m1.newsq==m2.newsq) && (m1.oldpc==m2.oldpc))
type board
  squares::Array{Int,1}
  pieces::Array{Int,1}
  moves::Array{moves,1}
  whitepoints::Int
  blackpoints::Int
  whitesmove::Bool
end
function newboard()
  squares = [collect(1:16); NOSQ*ones(Int,32); collect(49:64)]
  pieces = [collect(1:16); zeros(Int,32); collect(49:64); 0]
  moves = Array{move,1}()
  whitepoints = +1039
  blackpoints = -1039
  whitesmove = true
  board(squares,pieces,moves,whitepoints,blackpoints,whitesmove)
end
value(b::board) = sum(VALUES[b.pieces .!= NOSQ])
function apply!(b::board, m::move)
  b.pieces[m.newpc]=m.newsq
  b.squares[m.oldsq]=NOSQ
  b.squares[m.newsq]=m.newpc
  b.whitesmove=!b.whitesmove
  return
end
function apply!(b::board, m::take)
  b.pieces[m.newpc]=m.newsq
  b.squares[m.oldsq]=NOSQ
  b.squares[m.newsq]=m.newpc
  b.pieces[m.takes]=NOSQ
  b.whitesmove=!b.whitesmove
  return
end
function apply!(b::board, m::castle)
  b.pieces[m.oldpc] = m.newsq
  b.squares[m.oldsq] = NOSQ
  b.squares[m.newsq] = m.oldpc
  b.pieces[m.oldpc2] = m.newsq2
  b.squares[m.oldsq2] = NOSQ
  b.squares[m.newsq2] = m.oldpc2
  b.whitesmove=!b.whitesmove
  return
end
function apply!(b::board, m::promote)
  b.pieces[m.oldpc]=NOSQ
  b.pieces[m.newpc]=m.newsq
  b.squares[m.oldsq]=NOSQ
  b.squares[m.newsq]=m.newpc
  b.pieces[m.takes]=NOSQ
  b.whitesmove=!b.whitesmove
  return
end
function takeback!(b::board, m::move)
  undo = move(m.newsq,m.oldsq,m.newpc,m.oldpc)
  apply!(b, undo)
end
function takeback!(b::board, m::take)
  undo = move(m.newsq,m.oldsq,m.newpc,m.oldpc)
  taken = m.takes
  apply!(b, undo)
  b.pieces[taken] = undo.oldsq
  b.squares[undo.oldsq] = taken
end
function takeback!(b::board, m::castle)
  undo = castle(m.newsq, m.oldsq, m.oldpc, m.newsq2, m.oldsq2, m.oldpc2)
  apply!(b, undo)
  return
end
function takeback!(b::board, m::promote)
  undo = move(m.newsq,m.oldsq,m.newpc,m.oldpc)
  taken = m.takes
  apply!(b, undo)
  if taken != NOSQ
    b.pieces[taken] = undo.oldsq
    b.squares[undo.oldsq] = taken
  end
end
function takeback!(b::board)
  m = pop!(b.moves)
  takeback!(b, m)
end

LOOKUPTABLE = 99999*ones(Int,HTSIZE,LEVEL)
hashboard(b::board) = reinterpret(Int,hash(b.squares)+hash(b.whitesmove)) % div(HTSIZE,2) + div(HTSIZE,2) + 1

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

up(x::Int) = x<1||x>57 ? 0 : x+8
down(x::Int) = x<9||x>64 ? 0 : x-8
left(x::Int) = x<1||x>64||mod(x-1,8)==0 ? 0 : x-1
right(x::Int) = x<1||x>64||mod(x+1,8)==1 ? 0 : x+1
upLeft(x::Int) = x<1||x>57||mod(x-1,8)==0 ? 0 : x+7
upRight(x::Int) = x<1||x>57||mod(x+1,8)==1 ? 0 : x+9
downLeft(x::Int) = x<10||x>64||mod(x-1,8)==0 ? 0 : x-9
downRight(x::Int) = x<8||x>64||mod(x+1,8)==1 ? 0 : x-7
upUpLeft(x::Int) = mod(x+15,8)==0||x>49||x<1 ? 0 : x+15
upUpRight(x::Int) = mod(x+17,8)==1||x>47||x<1 ? 0 : x+17
upLeftLeft(x::Int) = mod(x+6,8) in [0,7]||x>58||x<1 ? 0 : x+6
upRightRight(x::Int) = mod(x+10,8) in [1,2]||x>54||x<1 ? 0 : x+10
downLeftLeft(x::Int) = mod(x-10,8) in [0,7]||x<11||x>64 ? 0 : x-10
downRightRight(x::Int) = mod(x-6,8) in [1,2]||x<7||x>64 ? 0 : x-6
downDownLeft(x::Int) = mod(x-17,8)==0||x<18||x>64 ? 0 : x-17
downDownRight(x::Int) = mod(x-15,8)==1||x<16||x>64 ? 0 : x-15

_up(x) = x+8
_down(x) = x-8
_left(x) = x-1
_right(x) = x+1
_upLeft(x) = x+7
_upRight(x) = x+9
_downLeft(x) = x-9
_downRight(x) = x-7
_upUpLeft(x) = x+15
_upUpRight(x) = x+17
_upLeftLeft(x) = x+6
_upRightRight(x) = x+10
_downLeftLeft(x) = x-10
_downRightRight(x) = x-6
_downDownLeft(x) = x-17
_downDownRight(x) = x-15

pawnUnmoved(p,b) = ((9<=p<=16 || 49<=p<=56) && b.pieces[p]==p)

row(x) = div((x-1)%64,8)+1
col(x) = mod((x-1)%64,8)+1

isempty(x::Int, b::board) = 0<x<65 && b.squares[x]==NOSQ
iswhite(x::Int, b::board) = 0<x<65 && 0<b.squares[x]<33
isblack(x::Int, b::board) = 0<x<65 && 32<b.squares[x]<NOSQ

#isempty(x::Int, b::board) = b.squares[x]==NOSQ
#iswhite(x::Int, b::board) = 0<b.squares[x]<33
#isblack(x::Int, b::board) = 32<b.squares[x]<NOSQ

# string manipulations down here
PIECES = ["wR","wN","wB","wQ","wK","wB","wN","wR",
          "wP","wP","wP","wP","wP","wP","wP","wP",
          "wQ","wQ","wQ","wQ","wQ","wQ","wQ","wQ",
          "wN","wN","wN","wN","wN","wN","wN","wN",
          "bN","bN","bN","bN","bN","bN","bN","bN",
          "bQ","bQ","bQ","bQ","bQ","bQ","bQ","bQ",
          "bP","bP","bP","bP","bP","bP","bP","bP",
          "bR","bN","bB","bQ","bK","bB","bN","bR",
          "  "]
VALUES = [+5;+3;+3;+9;+1000;+3;+3;+5;
          +1*ones(Int,8);
          +9*ones(Int,8);
          +3*ones(Int,8);
          -3*ones(Int,8);
          -9*ones(Int,8);
          -ones(Int,8);
          -5;-3;-3;-9;-1000;-3;-3;-5;
          0]

# new board representation with stones
NEWPIECES = ["OO","OO","OO","OO","OO","OO","OO","OO","OO","OO",
             "OO","OO","OO","OO","OO","OO","OO","OO","OO","OO",
             "OO","wR","wN","wB","wQ","wK","wB","wN","wR","OO",
             "OO","wP","wP","wP","wP","wP","wP","wP","wP","OO",
             "OO","wQ","wQ","wQ","wQ","wQ","wQ","wQ","wQ","OO",
             "OO","wN","wN","wN","wN","wN","wN","wN","wN","OO",
             "OO","bN","bN","bN","bN","bN","bN","bN","bN","OO",
             "OO","bQ","bQ","bQ","bQ","bQ","bQ","bQ","bQ","OO",
             "OO","bP","bP","bP","bP","bP","bP","bP","bP","OO",
             "OO","bR","bN","bB","bQ","bK","bB","bN","bR","OO",
             "OO","OO","OO","OO","OO","OO","OO","OO","OO","OO",
             "OO","OO","OO","OO","OO","OO","OO","OO","OO","OO",
             "  "]
NEWVALUES = [zeros(Int,10);
             zeros(Int,10);
             0;+5;+3;+3;+9;+1000;+3;+3;+5;0;
             0;+1*ones(Int,8);0;
             0;+9*ones(Int,8);0;
             0;+3*ones(Int,8);0;
             0;-3*ones(Int,8);0;
             0;-9*ones(Int,8);0;
             0;-ones(Int,8);0;
             0;-5;-3;-3;-9;-1000;-3;-3;-5;0;
             zeros(Int,10);
             zeros(Int,10);
             0]
colstr(x) = ["A","B","C","D","E","F","G","H"][col(x)]
colnum(x) = parse(Int, x) - 9
rowstr(x) = string(row(x))
square(x) = string(colstr(x),rowstr(x))
isopposite(whitesmove, x, b) = whitesmove ? isblack(x, b) : iswhite(x, b)
isblack(piece) = piece[1]=='b'
iswhite(piece) = piece[1]=='w'

function tomove(b::board)
  function _tomove(m)
    if length(m) == 4
        m = string(m[1:2],":",m[3:4])
    end
    fromcol,fromrow,tocol,torow = colnum(m[1]),parse(Int,m[2]),colnum(m[4]),parse(Int,m[5])
    from = fromcol + 8*(fromrow-1)
    to = tocol + 8*(torow-1)
    piece = b.squares[from]
    takes = b.squares[to]
    if piece in [whiteking; blackking]
        if abs(from-to)==2
            return first([x for x in castlingmoves if x.oldsq==from && x.newsq==to])
        end
    end 
    if length(m)==7 
        newpcstr = m[7]
        if (piece in whitepawn) && (row(to)==8)
            newpc = newpcstr=='N' ? piece+16 : piece+8
            return promote(from,to,piece,newpc,b.squares[to]) 
        elseif (piece in blackpawn) && (row(to)==1)
            newpc = newpcstr=='N' ? piece-16 : piece-8
            return promote(from,to,piece,newpc,b.squares[to])
        end
    end
    if takes == NOSQ
      return move(from,to,piece,piece)
    else
      return take(from,to,piece,piece,takes)
    end
  end
end

# TODO: split back into whitePawnMoves and blackPawnMoves.
function pawnMoves(b::board; whitesmove=b.whitesmove)
  mymoves = moves[]
  inc = whitesmove ? up : down
  pieces = whitesmove ? whitepawn : blackpawn
  for piece in pieces
    from = b.pieces[piece]
    if from != NOSQ
      for to in [inc(left(from)), inc(right(from))]
        if isopposite(whitesmove,to,b)
          if (whitesmove && row(to)==8)
            push!(mymoves,promote(from,to,piece,piece+08,b.squares[to]))
            push!(mymoves,promote(from,to,piece,piece+16,b.squares[to]))
          elseif (!whitesmove && row(to)==1)
            push!(mymoves,promote(from,to,piece,piece-08,b.squares[to]))
            push!(mymoves,promote(from,to,piece,piece-16,b.squares[to]))
          else
            push!(mymoves,take(from,to,piece,piece,b.squares[to]))
          end
        end
      end
      to = inc(from)
      if isempty(to,b)
        if (whitesmove && row(to)==8)
          push!(mymoves,promote(from,to,piece,piece+08,NOSQ))
          push!(mymoves,promote(from,to,piece,piece+16,NOSQ))
        elseif (!whitesmove && row(to)==1)
          push!(mymoves,promote(from,to,piece,piece-08,NOSQ))
          push!(mymoves,promote(from,to,piece,piece-16,NOSQ))
        else
          push!(mymoves,move(from,to,piece,piece))
        end
        to = inc(to)
        if pawnUnmoved(piece,b) && isempty(to,b)
          push!(mymoves,move(from,to,piece,piece))
        end
      end
    end
  end
  mymoves
end

knightIncrements = [upUpLeft upUpRight upLeftLeft upRightRight downLeftLeft downRightRight downDownLeft downDownRight]
function knightMoves(b::board; whitesmove=b.whitesmove)
  mymoves = moves[]
  pieces = whitesmove ? whiteknight : blackknight
  for piece in pieces
    from = b.pieces[piece]
    if from != NOSQ
      for m in knightIncrements
        to = m(from)
        if isempty(to,b)
          push!(mymoves,move(from,to,piece,piece))
        elseif isopposite(whitesmove,to,b)
          push!(mymoves,take(from,to,piece,piece,b.squares[to]))
        end
      end
    end
  end
  mymoves
end

function crossboard(b::board, pieces, whitesmove, increments, multistep)
  mymoves = moves[]
  for piece in pieces
    from = b.pieces[piece]
    if from != NOSQ
      for inc in increments
        to = from
        while true
          to = inc(to)
          if isempty(to,b)
            push!(mymoves,move(from,to,piece,piece))
            if !multistep
              break
            end
          elseif isopposite(whitesmove,to,b)
            push!(mymoves,take(from,to,piece,piece,b.squares[to]))
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


rookIncrements = [up down left right]
function rookMoves(b::board; whitesmove=b.whitesmove)
    pieces = whitesmove ? whiterook : blackrook
    crossboard(b, pieces, whitesmove, rookIncrements, true)
end

bishopIncrements = [upLeft upRight downLeft downRight]
function bishopMoves(b::board; whitesmove=b.whitesmove)
    pieces = whitesmove ? whitebishop : blackbishop
    crossboard(b, pieces, whitesmove, bishopIncrements, true)
end

queenIncrements = [up down left right upLeft upRight downLeft downRight]
function queenMoves(b::board; whitesmove=b.whitesmove)
    pieces = whitesmove ? whitequeen : blackqueen
    crossboard(b, pieces, whitesmove, queenIncrements, true)
end

kingIncrements = [up down left right upLeft upRight downLeft downRight]
function kingMoves(b::board; whitesmove=b.whitesmove)
    pieces = whitesmove ? whiteking : blackking
    crossboard(b, pieces, whitesmove, kingIncrements, false)
end

function castlingMoves(b::board, possible::Array{moves,1}; whitesmove=b.whitesmove)
    mymoves = moves[]
    pieces = whitesmove ? whiteking : blackking
    unmoved(piece) = !(piece in [m.oldpc for m in b.moves])
    if whitesmove && unmoved(05)
        if (move(05,04,05,05) in possible) && (move(01,04,01,01) in possible) && unmoved(01)
            #if !incheck(b, whitesmove)
                push!(mymoves, castle(05,03,05,01,04,01))
            #end
        end
        if (move(05,06,05,05) in possible) && (move(08,06,08,08) in possible) && unmoved(08)
            #if !incheck(b, whitesmove)
                push!(mymoves, castle(05,07,05,08,06,08))
            #end
        end
    elseif !whitesmove && unmoved(61)
        if (move(61,60,61,61) in possible) && (move(57,60,57,57) in possible) && unmoved(57)
            #if !incheck(b, whitesmove)
                push!(mymoves, castle(61,59,61,57,60,57))
            #end
        end
        if (move(61,62,61,61) in possible) && (move(64,62,64,64) in possible) && unmoved(64)
            #if !incheck(b, whitesmove)
                push!(mymoves, castle(61,63,61,64,62,64))
            #end
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
    k = white ? 5 : 61
    b.whitesmove = !white
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
    takeback!(b, m)
    return result
end

function allowedmoves(b::board)
    [m for m in possiblemoves(b) if !(intocheck(b, m))]
end 

function alphabeta(bi::board, depth, α, β, whitesmove; options=moves[])
  prescribed = length(options) !== 0
  toconsider = prescribed ? options : possiblemoves(bi)
  if depth == 0
    return value(bi), move(0, 0, 0, 0)
  end
  if whitesmove
    mb, vb = move(0, 0, 0, 0), -Inf
    for mi in toconsider
      apply!(bi, mi)
      push!(bi.moves, mi)
      s, mr = alphabeta(bi,depth-1,α,β,false)
      takeback!(bi)
      if s > vb
        vb, mb = s, mi
      end
      α = max(α, vb)
      if β <= α
        break
      end
    end
    VERBOSE && println("whitesmove: ",depth," :  ","    "^(LEVEL-depth),show(mb), " : ", v, " α=$α β=$β")
    return vb, mb
  else
    mb, vb = move(0, 0, 0, 0), +Inf
    for mi in toconsider
      apply!(bi, mi)
      push!(bi.moves, mi)
      s, mr = alphabeta(bi,depth-1,α,β,true)
      takeback!(bi)
      if s < vb
        vb, mb = s, mi
      end
      β = min(β, vb)
      if β <= α
        break
      end
    end
    VERBOSE && println("blacksmove ",depth," :  ","    "^(LEVEL-depth),show(mb), " : ", v, " α=$α β=$β")
    return vb, mb
  end
end

function iterativelydeepen(b::board, depth, iterations; options=moves[])
    m, s = move(0, 0, 0, 0), -Inf
    for i in 1:iterations
        VERBOSE && println("Running alphabeta at $(LEVEL-iterations+i) ply")
        s, m = alphabeta(b, depth-iterations+i, -Inf, Inf, b.whitesmove; options=options)
        #s, m = @parallel (max) for option in options
        #    alphabeta(b, depth-iterations+i, -Inf, Inf, b.whitesmove; options=[option])
        #end
    end
    return m, s
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
  blackTaken = [PIECES[x] for x in taken[taken.>32]]
  whiteTaken = [PIECES[x] for x in taken[taken.<17]]
  blackPrisoners = join(whiteTaken,", ")
  whitePrisoners = join(blackTaken,", ")
  print_with_color(:red,"\n   =========================\n")
  for row in 8:-1:1
    print_with_color(:blue, "$row | ")
    for col in 1:8
      piecestr = PIECES[b.squares[col+(row-1)*8]]
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

show(m::move) = string(square(m.oldsq),":",square(m.newsq))
show(m::take) = string(square(m.oldsq),"x",square(m.newsq))
show(m::castle) = string(square(m.oldsq),":",square(m.newsq))
show(m::promote) = string(square(m.oldsq),ifelse(m.takes!=NOSQ,"x",":"),square(m.newsq),":",ifelse(25<=m.newpc<=42,"N","Q"))

function gameover(b)
    winner = ifelse(b.whitesmove,"BLACK","WHITE")
    if incheck(b, b.whitesmove)
        return "$winner WINS!"
    else
        return "DRAWN GAME!"
    end
end

function play(b; autoplay=false)
  while true
    b2 = deepcopy(b)
    allowed = allowedmoves(b)
    tic = time()
    try
      if !autoplay && b.whitesmove
        print(b)
        if length(allowed) == 0
           return gameover(b)
        end
        mstr = input("\n> ")
        if mstr in ["back","takeback","undo"]
            takeback!(b); takeback!(b)
            println("\nYa cheetah! Rewinding your last move...")
            continue
        else
            assert(tomove(b)(mstr) in allowed)
            m = tomove(b)(mstr)
            toc = time()
            elapsed = Base.Dates.Second(round(toc-tic))
            print(show(m)," elapsed time: $elapsed\n")
            apply!(b, m)
            push!(b.moves, m)
        end
      end
      print(b)
      b2 = deepcopy(b)
      allowed = shuffle(allowedmoves(b2))
      tic = time()
      if length(allowed) == 0
         return gameover(b2)
      end
      m, s = iterativelydeepen(b2, LEVEL, ITDEEP; options=allowed)
      toc = time()
      elapsed = Base.Dates.Second(round(toc-tic))
      print("\n> ",show(m)," elapsed time: $elapsed\n")
      sleep(1)
      apply!(b, m)
      push!(b.moves, m)
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

function replay(b::board)
    b2 = newboard()
    moves = reverse(deepcopy(b.moves))
    while length(moves) > 0
      try
        m = pop!(moves)
        println("> $(show(m))")
        apply!(b2, m)
        print(b2)
        if length(moves) > 0
            println("\nReplay mode: press enter for next move")
            readline()
        end
      catch
        return b2
      end
    end
    println("That's all folks!")
end

b = try
   b
catch
   newboard()
end
play(b, autoplay=AUTOPLAY)
