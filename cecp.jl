#!/usr/bin/env julia

#include("chess.jl")

LOG=open("cecp.log","w")

function getcommand()
    cmd=readline()
    write(LOG,"> $(cmd)\n")
    flush(LOG)
    return cmd
end

function sendcommand(cmd)
    write(STDOUT,cmd)
    write(STDOUT,"\n")
    flush(STDOUT)
    write(LOG,"$(cmd)\n")
    flush(LOG)
end

function log(text)
    write(LOG,"# $(text)\n")
    flush(LOG)
end

function nextside(side)
    if side==:white
        return :black
    end
    return :white
end

function xboard()
    #game=[(Chess.Board(),:white)]
    play=:black
    
    while true
        fullcmd=getcommand()
        cmds=split(fullcmd)
        if length(cmds)>0
            cmd=cmds[1]
            if cmd=="protover"
                sendcommand("feature ping=1 setboard=1 playother=1 usermove=1 sigint=0 sigterm=0")
            end
            if cmd=="new"
                #game=[(Chess.Board(),:white)]
                play=:black
            end
            if cmd=="ping"
                sendcommand("pong $(cmds[2])")
            end
            if cmd=="usermove"
                textmove=cmds[2]
                #moves=Chess.usermoves(game[end]...)
                if haskey(moves,textmove)
                    #push!(game,(Chess.makemove(game[end][1],moves[textmove]),nextside(game[end][2])))
                else
                    sendcommand("Illegal move: $(textmove)")
                end
            end
            if cmd=="quit"
                break
            end
            if cmd=="remove"
                #game=game[1:end-2]
            end
        end
        if play==:black #game[end][2]
            #m=if play==:white
            #    #Chess.searchwhite(game[end][1],6)[1]
            #else
            #    #Chess.searchblack(game[end][1],6)[1]
            #end
            #textmove=Chess.move2alg(m)
            textmove = "e7e6\n"
            sendcommand("move $(textmove)")
            #push!(game,(Chess.makemove(game[end][1],m),nextside(game[end][2])))
        end
    end
end

xboard()

