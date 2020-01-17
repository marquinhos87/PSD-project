
-module(frontend).
-export([run/1]).

-include("nefitproto.hrl").

run(Port) ->
    Arbiters = maps:new(), % Inicializar os arbiters depois...
    State = spawn(fun()-> globalState(
        maps:new(),
        maps:new(),
        Arbiters)
                  end),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 4}, {reuseaddr, true}]),
    acceptor(LSock, State).

% accepts connections
acceptor(LSock, State) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, State) end),
    connectedClient(Sock, State).

globalState(RegisteredUsers, ConnectedUsers, Arbiters) ->
    receive
        {Sock, login, Name, Pass, Type} ->
            Status = maps:find(Name,RegisteredUsers),
            io:format("Status->~w",[Status]),
            case Status of
                {ok, Value} ->
                    io:format("Value->~w",[Value]),
                    if
                        Value == Pass ->
                            Connected = maps:put(Name,Sock,ConnectedUsers),
                            Sock ! {success, Type},
                            globalState(RegisteredUsers, Connected, Arbiters);
                        true ->
                            Sock ! {failure},
                            globalState(RegisteredUsers, ConnectedUsers, Arbiters)
                    end;
                error ->
                    Sock ! {failure}
            end;

        {Sock, register, Name, Pass, _} ->
            Status = maps:find(Name,RegisteredUsers),
            io:format("Status->~w",[Status]),
            case Status of
                {ok, _} ->
                    Sock ! {failure},
                    globalState(RegisteredUsers, ConnectedUsers, Arbiters);
                error ->
                    io:format("4"),
                    Registered = maps:put(Name, Pass, RegisteredUsers),
                    Sock ! {successR},
                    globalState(Registered, ConnectedUsers, Arbiters)
            end;

        {order, Pid, Manuf, Product, Quant, Value} ->
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
            {Sock,I} = for(maps:size(Arbiters),"c",99999999,maps:iterator(Arbiters)),
            Map = maps:put(Sock,I+1),
            Sock ! {disponibility, Msg},
            globalState(RegisteredUsers, ConnectedUsers, Map);

        {production, M,P,Q,V} ->
            Msg = #'ProductionM'{nameP = P,quant = Q, value = V},
            {Sock,_} = maps:get(M,ConnectedUsers),
            Sock ! {production, Msg},
            globalState(RegisteredUsers,ConnectedUsers,Arbiters);

        {result, R,M,I} ->
            Msg = #'ResultI'{result = R, msg = M},
            {Sock,_} = maps:get(I,ConnectedUsers),
            Sock ! {result, Msg},
            globalState(RegisteredUsers,ConnectedUsers,Arbiters);

        {ack,A,M,I} ->
            Msg = #'OrderAckI'{ack = A, msg = M},
            {Sock,_} = maps:get(I,ConnectedUsers),
            Sock ! {ack, Msg},
            globalState(RegisteredUsers,ConnectedUsers,Arbiters);
        
        {info,M,P,Min,Max,V,Pe,I} ->
            Msg = #'InfoI'{nameM = M, nameP = P,maximun = Max, minimun = Min, value = V, period = Pe},
            {Sock,_} = maps:get(I,ConnectedUsers),
            Sock ! {info, Msg},
            globalState(RegisteredUsers,ConnectedUsers,Arbiters)
    end.

for(0,Sock,I,_) -> {Sock,I};
for(N,Sock,I,Arbiters) ->
    {A,B,C} = maps:next(Arbiters),
    if
        B < I -> for(N-1,A,B,C);
        true -> for(N-1,Sock,I,C)
    end.

% treat data received from arbiters
arbiter(Sock, State) ->
    receive
        {tcp, _, Data} ->
            Msg = decode(Data),
            Field = Msg#'Server'.msg,
            case Field of
                {m6, Production} ->
                    State ! {production,
                        Production#'ProductionS'.nameM,
                        Production#'ProductionS'.nameP,
                        Production#'ProductionS'.quant,
                        Production#'ProductionS'.value},
                    arbiter(Sock, State);
                {m4, Result} ->
                    State ! {result,
                        Result#'ResultS'.result,
                        Result#'ResultS'.msg,
                        Result#'ResultS'.nameI},
                    arbiter(Sock, State);
                {m7, Ack} ->
                    State ! {ack,
                        Ack#'OrderAckS'.ack,
                        Ack#'OrderAckS'.msg,
                        Ack#'OrderAckS'.nameI},
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
                    State ! {order,Sock,
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
            io:format("Msg => ~w~n",[Msg]),
            case Msg#'MsgAuth'.mtype of
                'REGISTER' ->
                    io:format("1~n"),
                    State ! {Sock,
                        register,
                        Msg#'MsgAuth'.name,
                        Msg#'MsgAuth'.pass,
                        Msg#'MsgAuth'.ctype};
                'LOGIN' ->
                    io:format("2~n"),
                    State ! {Sock,
                        login,
                        Msg#'MsgAuth'.name,
                        Msg#'MsgAuth'.pass,
                        Msg#'MsgAuth'.ctype}
            end,
            io:format("Chega aqui~n"),
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
            io:format("3~n"),
            Msg = #'MsgAck'{ok = true},
            M = nefitproto:encode_msg(Msg),
            gen_tcp:send(Sock, M),
            io:format("3~n"),
            connectedClient(Sock, State);
        {failure} ->
            Msg = #'MsgAck'{ok = false},
            M = nefitproto:encode_msg(Msg),
            gen_tcp:send(Sock, M),
            io:format("3~n");
        {tcp_closed, _} ->
            io:format("Closed.");
        {tcp_error, _, _} ->
            io:format("Error.")
    end.

% serialize


% deserialize
decode(Data) ->
    Details = nefitproto:decode_msg(Data, 'Server'),
    Details.

decode(Data, Type) ->
    Details = nefitproto:decode_msg(Data, Type),
    Details.
