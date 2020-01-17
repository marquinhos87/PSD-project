
-module(frontend).
-export([run/1]).

-include("nefitproto.hrl").

run(Port) ->
    Authenticator = spawn(fun()-> authenticator(maps:new()) end),
    Negotiations = spawn(fun()-> treatNegotiation(maps:new()) end),
    Orders = spawn(fun()-> treatOrder([], Negotiations) end),
    Productions = spawn(fun()-> treatProduction([],[],Negotiations) end),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),
    acceptor(LSock, Authenticator, Orders, Productions).

% accepts connections
acceptor(LSock, Authenticator, Orders, Productions) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, Authenticator, Orders, Productions) end),
    connectedClient(Sock, Authenticator, Orders, Productions).

% process responsible for article orders
treatOrder(Orders, Negotiations) ->
    receive
        {order, Pid, Manuf, Product, Quant, Value} ->
            Msg = #'OrderN'{nameM = Manuf, nameP = Product, quant = Quant, value = Value},
            nefitproto:encode_msg(Msg),
            Negotiations ! {order, Pid, Msg},
            treatOrder(Orders, Negotiations)
    end.

% process responsible for article productions
treatProduction(Productions, Subs, Negotiations) ->
    receive
        {production, Sock, Manuf, Product, Value, Min, Max, Period} ->
            Msg = #'DisponibilityN'{nameP = Product, nameM = Manuf, value = Value, minimun = Min, maximun = Max,
                period = Period},
            nefitproto:encode_msg(Msg),
            Negotiations ! {disponibility, Sock, Msg},
            % percorrer os sockets guardados em Subs para mandar notificacao de nova producao de um fabricante
            treatProduction(Productions, Subs, Negotiations)
    end.

% process responsible for the results received by the arbiters
treatNegotiation(Arbiters) ->
    receive
        {order, Importer, Msg} ->
            % send the order to the arbiter with the least amount of load
            treatNegotiation(Arbiters)
    end.

% treat data received from arbiters
arbiter(Sock, Negotiations) ->
    receive
        {tcp, _, Data} ->
            Msg = decode(Data),
            Field = Msg#'Server'.msg,
            case Field of
                {m6, Production} ->
                    Negotiations ! {Sock,
                        Production#'ProductionS'.nameM,
                        Production#'ProductionS'.nameP,
                        Production#'ProductionS'.quant,
                        Production#'ProductionS'.value},
                    arbiter(Sock, Negotiations);
                {m4, Result} ->
                    Negotiations ! {Sock,
                        Result#'ResultS'.result,
                        Result#'ResultS'.msg,
                        Result#'ResultS'.nameI},
                    arbiter(Sock, Negotiations);
                {m7, Ack} ->
                    Negotiations ! {Sock,
                        Ack#'OrderAckS'.ack,
                        Ack#'OrderAckS'.msg,
                        Ack#'OrderAckS'.nameI},
                    arbiter(Sock, Negotiations);
                {m5, Info} ->
                    Negotiations ! {Sock,
                        Info#'InfoS'.nameM,
                        Info#'InfoS'.nameP,
                        Info#'InfoS'.minimun,
                        Info#'InfoS'.maximun,
                        Info#'InfoS'.value,
                        Info#'InfoS'.period,
                        Info#'InfoS'.nameI},
                    arbiter(Sock, Negotiations)
            end;
        {order, Msg} ->
            gen_tcp:send(Sock, Msg),
            arbiter(Sock, Negotiations)
    end.

% treats client logged as manufacturer
manufacturer(Sock, Productions) ->
    receive
        {tcp, _, Data} ->
            Msg = decode(Data),
            Field = Msg#'Server'.msg,
            case Field of
                {m1, ProductionOffer} ->
                    Productions ! {production, Sock,
                        ProductionOffer#'DisponibilityS'.nameM,
                        ProductionOffer#'DisponibilityS'.nameP,
                        ProductionOffer#'DisponibilityS'.value,
                        ProductionOffer#'DisponibilityS'.minimun,
                        ProductionOffer#'DisponibilityS'.maximun,
                        ProductionOffer#'DisponibilityS'.period}
        end;
        {tcp_closed, _} ->
            io:format("Closed.");
        {tcp_error, _, _} ->
            io:format("Error.")
    end.

% treats client logged as importer
importer(Sock, Orders, Productions) ->
    receive
        {tcp, _, Data} ->
            Msg = decode(Data),
            Field = Msg#'Server'.msg,
            case Field of
                {m2,Order} ->
                    Orders ! {order,Sock,
                        Order#'OrderS'.nameM,
                        Order#'OrderS'.nameP,
                        Order#'OrderS'.quant,
                        Order#'OrderS'.value},
                    importer(Sock, Orders, Productions);
                {m3,Sub} ->
                    Productions ! {sub, Sock, Sub#'SubS'.subs},
                    importer(Sock, Orders, Productions);
                {m8, Get} ->
                    % to do
                    importer(Sock, Orders, Productions)
            end;
        {tcp_closed, _} ->
            io:format("Closed.");
        {tcp_error, _, _} ->
            io:format("Error.")
    end.

% registers or logs in the connected client
connectedClient(Sock, Authenticator, Orders, Productions) ->
    io:format("Chegou aqui~n"),
    receive
        {tcp, _, Data} ->
            io:format("Recebeu autenticacao~n"),
            Msg = nefitproto:decode_msg(Data,'MsgAuth'),
            Authenticator ! {Sock,
                Msg#'MsgAuth'.mtype,
                Msg#'MsgAuth'.name,
                Msg#'MsgAuth'.pass,
                Msg#'MsgAuth'.ctype},
            connectedClient(Sock, Authenticator, Orders, Productions);
        {success, login, Type} ->
            Msg = #'MsgAck'{ok = true},
            gen_tcp:send(Sock, Msg),
            case Type of
                'IMPORTER'-> importer(Sock, Orders, Productions);
                'MANUFACTURER'-> io:format("Login Manufacturer~n"),manufacturer(Sock, Productions)
            end;
        {success, register} ->
            Msg = #'MsgAck'{ok = true},
            gen_tcp:send(Sock, Msg),
            connectedClient(Sock, Authenticator, Orders, Productions);
        {failure} ->
            Msg = #'MsgAck'{ok = false},
            gen_tcp:send(Sock, Msg);
        {tcp_closed, _} ->
            io:format("Closed.");
        {tcp_error, _, _} ->
            io:format("Error.")
    end.

% process responsible for authenticating and maybe register users, might separate
authenticator(RegisteredUsers) ->
    receive
        {Sock, 'LOGIN', Name, Pass, Type} ->
            Status = maps:find(Name,RegisteredUsers),
            case Status of
                {ok, Value} ->
                    if
                        Value == Pass ->
                            Sock ! {success, login, Type},
                            authenticator(RegisteredUsers);
                        true ->
                            Sock ! {failure},
                            authenticator(RegisteredUsers)
                    end;
                error ->
                    Sock ! {failure}
            end;
        {Sock, 'REGISTER', Name, Pass, _} ->
            Status = maps:find(Name,RegisteredUsers),
            case Status of
                {ok, _} ->
                    Sock ! {failure},
                    authenticator(RegisteredUsers);
                error ->
                    Registered = maps:put(Name, Pass, RegisteredUsers),
                    Sock ! {success, register},
                    authenticator(Registered)
            end
    end.

% serialize


% deserialize
decode(Data) ->
    Details = nefitproto:decode_msg(Data, 'Server'),
    Details.

decode(Data, Type) ->
    Details = nefitproto:decode_msg(Data, Type),
    Details.
