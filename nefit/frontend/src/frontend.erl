
-module(frontend).
-export([run/1]).

-include("nefitproto.hrl").

run(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 4}, {reuseaddr, true}]),
    State = spawn(fun()-> globalState(
        maps:new(),
        maps:new(),
        maps:new())
                  end),
    acceptorA(2,LSock,State),
    acceptor(LSock, State).

%accepts arbiters connections
acceptorA(N,LSock,State) when N > 0 ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptorA(N-1,LSock,State) end),
    State ! {arbiter, self()},
    arbiter(Sock,State).

% accepts connections
acceptor(LSock, State) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, State) end),
    connectedClient(Sock, State).

globalState(RegisteredUsers, ConnectedUsers, Arbiters) ->
    receive
        {Pid, login, Name, Pass, Type} ->
            Status = maps:find(Name,RegisteredUsers),
            case Status of
                {ok, Value} ->
                    if
                        Value == Pass ->
                            Connected = maps:put(Name,Pid,ConnectedUsers),
                            Pid ! {success, Type},
                            globalState(RegisteredUsers, Connected, Arbiters);
                        true ->
                            Pid ! {failure},
                            globalState(RegisteredUsers, ConnectedUsers, Arbiters)
                    end;
                error ->
                    Pid ! {failure}
            end;

        {Pid, register, Name, Pass, _} ->
            Status = maps:find(Name,RegisteredUsers),
            case Status of
                {ok, _} ->
                    Pid ! {failure},
                    globalState(RegisteredUsers, ConnectedUsers, Arbiters);
                error ->
                    Registered = maps:put(Name, Pass, RegisteredUsers),
                    Pid ! {successR},
                    globalState(Registered, ConnectedUsers, Arbiters)
            end;

        {order, Manuf, Product, Quant, Value} ->
            Msg = #'OrderN'{nameM = Manuf, nameP = Product, quant = Quant, value = Value},
            List = maps:to_list(Arbiters),
            [Arbiter ! {sub, Msg} || {Arbiter,_} <- List],
            globalState(RegisteredUsers, ConnectedUsers, Arbiters);

        {sub, Imp, Manuf} ->
            Msg = #'SubN'{nameI = Imp, subs = Manuf},
            List = maps:to_list(Arbiters),
            [Arbiter ! {sub, Msg} || {Arbiter,_} <- List],
            globalState(RegisteredUsers, ConnectedUsers, Arbiters);

        {disponibility, M, P, Value, Min, Max, Period} ->
            Msg = #'DisponibilityN'{nameM = M, nameP = P, value = Value, minimun = Min, maximun = Max, period = Period},
            {Pid,I} = for(maps:size(Arbiters),"c",99999999,maps:iterator(Arbiters)),
            Map = maps:put(Pid,I+1,Arbiters),
            Pid ! {disponibility, Msg},
            globalState(RegisteredUsers, ConnectedUsers, Map);

        {production,PidA, M,P,Q,V} ->
            Msg = #'ProductionM'{nameP = P,quant = Q, value = V},
            {PidU,_} = maps:get(M,ConnectedUsers),
            PidU ! {production, Msg},
            {_,I} = maps:get(PidA,Arbiters),
            Map = maps:put(PidA,I-1,Arbiters),
            globalState(RegisteredUsers,ConnectedUsers,Map);

        {result, R,M,I} ->
            Msg = #'ResultI'{result = R, msg = M},
            {Pid,_} = maps:get(I,ConnectedUsers),
            Pid ! {result, Msg},
            globalState(RegisteredUsers,ConnectedUsers,Arbiters);

        {ack,A,M,I} ->
            Msg = #'OrderAckI'{ack = A, msg = M},
            {Pid,_} = maps:get(I,ConnectedUsers),
            Pid ! {ack, Msg},
            globalState(RegisteredUsers,ConnectedUsers,Arbiters);

        {info,M,P,Min,Max,V,Pe,I} ->
            Msg = #'InfoI'{nameM = M, nameP = P,maximun = Max, minimun = Min, value = V, period = Pe},
            {Pid,_} = maps:get(I,ConnectedUsers),
            Pid ! {info, Msg},
            globalState(RegisteredUsers,ConnectedUsers,Arbiters);
        {arbiter, Pid} ->
            Map = maps:put(Pid,0,Arbiters),
            globalState(RegisteredUsers,ConnectedUsers,Map)
    end.

%aux function to discover the Arbiter with less active negotiations
for(0,Arbiter,I,_) -> {Arbiter,I};
for(N,Arbiter,I,Arbiters) ->
    {A,B,C} = maps:next(Arbiters),
    if
        B < I -> for(N-1,A,B,C);
        true -> for(N-1,Arbiter,I,C)
    end.

% treat data received from arbiters
arbiter(Sock, State) ->
    receive
        {tcp, _, Data} ->
            Msg = nefitproto:decode_msg(Data,'Server'),
            Field = Msg#'Server'.msg,
            case Field of
                {m4, Result} ->
                    State ! {result,
                        Result#'ResultS'.result,
                        Result#'ResultS'.msg,
                        Result#'ResultS'.nameI},
                    arbiter(Sock, State);
                {m5, Info} ->
                    State ! {info,
                        Info#'InfoS'.nameM,
                        Info#'InfoS'.nameP,
                        Info#'InfoS'.minimun,
                        Info#'InfoS'.maximun,
                        Info#'InfoS'.value,
                        Info#'InfoS'.period,
                        Info#'InfoS'.nameI},
                    arbiter(Sock, State);
                {m6, Production} ->
                    State ! {production, self(),
                        Production#'ProductionS'.nameM,
                        Production#'ProductionS'.nameP,
                        Production#'ProductionS'.quant,
                        Production#'ProductionS'.value},
                    arbiter(Sock, State);
                {m7, Ack} ->
                    State ! {ack,
                        Ack#'OrderAckS'.ack,
                        Ack#'OrderAckS'.msg,
                        Ack#'OrderAckS'.nameI},
                    arbiter(Sock, State)
            end;
        {order, Msg} ->
            MsgN = #'Negotiator'{msg = Msg},
            M = nefitproto:encode_msg(MsgN),
            gen_tcp:send(Sock, M),
            arbiter(Sock, State);
        {disponibility, Msg} ->
            MsgN = #'Negotiator'{msg = Msg},
            M = nefitproto:encode_msg(MsgN),
            gen_tcp:send(Sock, M),
            arbiter(Sock, State);
        {sub, Msg} ->
            MsgN = #'Negotiator'{msg = Msg},
            M = nefitproto:encode_msg(MsgN),
            gen_tcp:send(Sock, M),
            arbiter(Sock, State);
        {tcp_closed, _} ->
            io:format("Closed.");
        {tcp_error, _, _} ->
            io:format("Error.")
    end.

% treats client logged as manufacturer
manufacturer(Sock, State) ->
    receive
        {tcp, _, Data} ->
            Msg = decode(Data),
            Field = Msg#'Server'.msg,
            case Field of
                {m1, ProductionOffer} ->
                    State ! {disponibility,
                        ProductionOffer#'DisponibilityS'.nameM,
                        ProductionOffer#'DisponibilityS'.nameP,
                        ProductionOffer#'DisponibilityS'.value,
                        ProductionOffer#'DisponibilityS'.minimun,
                        ProductionOffer#'DisponibilityS'.maximun,
                        ProductionOffer#'DisponibilityS'.period}
            end,
            manufacturer(Sock, State);
        {production, Msg} ->
            M = nefitproto:encode_msg(Msg),
            gen_tcp:send(Sock,M),
            manufacturer(Sock,State);
        {tcp_closed, _} ->
            io:format("Closed.");
        {tcp_error, _, _} ->
            io:format("Error.")
    end.

% treats client logged as importer
importer(Sock, State) ->
    receive
        {tcp, _, Data} ->
            Msg = decode(Data),
            Field = Msg#'Server'.msg,
            case Field of
                {m2,Order} ->
                    State ! {order,
                        Order#'OrderS'.nameM,
                        Order#'OrderS'.nameP,
                        Order#'OrderS'.quant,
                        Order#'OrderS'.value},
                    importer(Sock, State);
                {m3,Sub} ->
                    State ! {sub, Sub#'SubS'.nameI ,Sub#'SubS'.subs},
                    importer(Sock, State)
            end;
        {ack, Msg} ->
            MsgI = #'Importer'{msg = Msg},
            M = nefitproto:encode_msg(MsgI),
            gen_tcp:send(Sock,M),
            importer(Sock,State);
        {result, Msg} ->
            MsgI = #'Importer'{msg = Msg},
            M = nefitproto:encode_msg(MsgI),
            gen_tcp:send(Sock,M),
            importer(Sock,State);
        {info, Msg} ->
            MsgI = #'Importer'{msg = Msg},
            M = nefitproto:encode_msg(MsgI),
            gen_tcp:send(Sock,M),
            importer(Sock,State);
        {tcp_closed, _} ->
            io:format("Closed.");
        {tcp_error, _, _} ->
            io:format("Error.")
    end.

% registers or logs in the connected client
connectedClient(Sock, State) ->
    receive
        {tcp, _, Data} ->
            Msg = nefitproto:decode_msg(Data,'MsgAuth'),
            case Msg#'MsgAuth'.mtype of
                'REGISTER' ->
                    State ! {self(),
                        register,
                        Msg#'MsgAuth'.name,
                        Msg#'MsgAuth'.pass,
                        Msg#'MsgAuth'.ctype};
                'LOGIN' ->
                    State ! {self(),
                        login,
                        Msg#'MsgAuth'.name,
                        Msg#'MsgAuth'.pass,
                        Msg#'MsgAuth'.ctype}
            end,
            connectedClient(Sock, State);
        {success, Type} ->
            Msg = #'MsgAck'{ok = true},
            M = nefitproto:encode_msg(Msg),
            gen_tcp:send(Sock, M),
            case Type of
                'IMPORTER'-> importer(Sock, State);
                'MANUFACTURER'-> manufacturer(Sock, State)
            end;
        {successR} ->
            Msg = #'MsgAck'{ok = true},
            M = nefitproto:encode_msg(Msg),
            gen_tcp:send(Sock, M),
            connectedClient(Sock, State);
        {failure} ->
            Msg = #'MsgAck'{ok = false},
            M = nefitproto:encode_msg(Msg),
            gen_tcp:send(Sock, M);
        {tcp_closed, _} ->
            io:format("Closed.");
        {tcp_error, _, _} ->
            io:format("Error.")
    end.
