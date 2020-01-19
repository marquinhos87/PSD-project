-module(frontend).
-export([run/1]).

-include("nefitproto.hrl").
-include("../_build/default/lib/chumak/include/chumak.hrl").

run(Port) ->
    application:start(chumak),
    {ok, Socket} = chumak:socket(pub),
    chumak:bind(Socket, tcp, "localhost", 12346),
    {ok, ListenSock} = gen_tcp:listen(Port, [binary, {packet, 4}, {reuseaddr, true}]),
    StatePid = spawn(fun() ->
        globalState(maps:new(), maps:new(), maps:new(), Socket, 0) end),
    acceptorA(0, ListenSock, StatePid).

% accepts arbiters connections
acceptorA(N, LSock, StatePid) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    StatePid ! {arbiter, self()},
    if
        N == 0 ->
            spawn(fun() -> acceptorA(N + 1, LSock, StatePid) end),
            arbiter(Sock, StatePid);
        true ->
            spawn(fun() -> acceptor(LSock, StatePid) end),
            arbiter(Sock, StatePid)
    end.

% accepts connections
acceptor(LSock, State) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, State) end),
    connectedClient(Sock, State).

globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos) ->
    receive
    % Send Message to a Client with info about his login
        {Pid, login, Name, Pass, Type} ->
            Status = maps:find(Name, RegisteredUsers),
            case Status of
                {ok, Value} ->
                    if
                        Value == Pass ->
                            Connected = maps:put(Name, Pid, ConnectedUsers),
                            Pid ! {success, Type},
                            globalState(RegisteredUsers, Connected, Arbiters, Socket, Pos);
                        true ->
                            Pid ! {failure},
                            globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos)
                    end;
                error ->
                    Pid ! {failure},
                    globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos)
            end;

    % Send Message to a Client with info about his register
        {Pid, register, Name, Pass} ->
            Status = maps:find(Name, RegisteredUsers),
            case Status of
                {ok, _} ->
                    Pid ! {failure},
                    globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos);
                error ->
                    Registered = maps:put(Name, Pass, RegisteredUsers),
                    Pid ! {successR},
                    globalState(Registered, ConnectedUsers, Arbiters, Socket, Pos)
            end;

    % Send Message to all Arbiter Actors with a new Order from an Importer
        {order, Manuf, Product, Quant, Value, Imp} ->
            Msg = #'OrderN'{nameM = Manuf, nameP = Product, quant = Quant, value = Value, nameI = Imp},
            MsgN = #'Negotiator'{msg = {order, Msg}},
            M = nefitproto:encode_msg(MsgN),
            Aux = string:concat(Manuf, Product),
            Str = binary:list_to_bin(Aux),
            Tam = string:length(Aux),
            %Neste momento rebenta por causa do Tam
            Mensagem = [Tam,Str, M],
            chumak:send(Socket, Mensagem),
            globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos + 1);

    % Send Message to all Arbiter Actors with the Manufacturers subscribed by an Importer
        {sub, Imp, Manuf} ->
            Msg = #'SubN'{nameI = Imp, subs = Manuf},
            List = maps:to_list(Arbiters),
            [Arbiter ! {sub, Msg} || {Arbiter, _} <- List],
            globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos);

    % Send Message to an Arbiter Actor with a new Product, the Arbiter chosen where the one with less Active Negotiations
        {disponibility, M, P, Value, Min, Max, Period} ->
            Msg = #'DisponibilityN'{nameM = M, nameP = P, value = Value, minimum = Min, maximum = Max, period = Period},
            {Pid, I} = for(maps:size(Arbiters), "c", 99999999, maps:iterator(Arbiters)),
            Map = maps:put(Pid, I + 1, Arbiters),
            Pid ! {disponibility, Msg},
            globalState(RegisteredUsers, ConnectedUsers, Map, Socket, Pos);

    % Send Message to a Manufacturer Actor with the Result about one of its products
        {production, PidA, M, P, Q, V} ->
            Msg = #'ServerToManufacturerSold'{productName = P, quantity = Q, unitPrice = V},
            PidU = maps:get(M, ConnectedUsers),
            PidU ! {production, Msg},
            I = maps:get(PidA, Arbiters),
            Map = maps:put(PidA, I - 1, Arbiters),
            globalState(RegisteredUsers, ConnectedUsers, Map, Socket, Pos);

        {noproduction, PidA, M, P} ->
            Msg = #'ServerToManufacturerNoOffers'{productName = P},
            PidU = maps:get(M, ConnectedUsers),
            PidU ! {noproduction, Msg},
            I = maps:get(PidA, Arbiters),
            Map = maps:put(PidA, I - 1, Arbiters),
            globalState(RegisteredUsers, ConnectedUsers, Map, Socket, Pos);

        {announced, M} ->
            Msg = #'ServerToManufacturerAnnounced'{},
            PidU = maps:get(M, ConnectedUsers),
            PidU ! {announced, Msg},
            globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos);

    % Send Message to an Importer Actor with an Result about one Product that his puts at least one Order
        {result, R, M, I} ->
            Msg = #'ResultI'{result = R, msg = M},
            Pid = maps:get(I, ConnectedUsers),
            Pid ! {result, Msg},
            globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos);

    % Send Message to an Importer Actor with an Ack About its Order
        {ack, A, M, I} ->
            Msg = #'OrderAckI'{ack = A, msg = M},
            Pid = maps:get(I, ConnectedUsers),
            Pid ! {ack, Msg},
            globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos);

    % Send Message to an Importer Actor with Info about a Product
        {info, M, P, Min, Max, V, Pe, I} ->
            Msg = #'InfoI'{nameM = M, nameP = P, maximum = Max, minimum = Min, value = V, period = Pe},
            Pid = maps:get(I, ConnectedUsers),
            Pid ! {info, Msg},
            globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos);

    % Add new Arbiter to the Arbiters Map
        {arbiter, Pid} ->
            Map = maps:put(Pid, 0, Arbiters),
            globalState(RegisteredUsers, ConnectedUsers, Map, Socket, Pos);

    % disconnect user when something went wrong
        {disconnectUser, Pid} ->
            Map = maps:remove(Pid, ConnectedUsers),
            globalState(RegisteredUsers, Map, Arbiters, Socket, Pos)
    end.

% aux function to discover the Arbiter with less active negotiations
for(0, Arbiter, I, _) -> {Arbiter, I};
for(N, Arbiter, I, Arbiters) ->
    {A, B, C} = maps:next(Arbiters),
    if
        B < I -> for(N - 1, A, B, C);
        true -> for(N - 1, Arbiter, I, C)
    end.

% treat data received from arbiters
arbiter(Sock, State) ->
    receive
        {tcp, _, Data} ->
            Msg = nefitproto:decode_msg(Data, 'Server'),
            Field = Msg#'Server'.msg,
            case Field of

                % Send Message to GlobalState Actor with a Result about a Product
                {m4, Result} ->
                    State ! {result,
                        Result#'ResultS'.result,
                        Result#'ResultS'.msg,
                        Result#'ResultS'.nameI},
                    arbiter(Sock, State);

                % Send Message to GlobalState Actor with an Info about a Product
                {m5, Info} ->
                    State ! {info,
                        Info#'InfoS'.nameM,
                        Info#'InfoS'.nameP,
                        Info#'InfoS'.minimum,
                        Info#'InfoS'.maximum,
                        Info#'InfoS'.value,
                        Info#'InfoS'.period,
                        Info#'InfoS'.nameI},
                    arbiter(Sock, State);

                % Send Message to GlobalState Actor with a Production about a Product
                {m6, Production} ->
                    State ! {production, self(),
                        Production#'ProductionS'.nameM,
                        Production#'ProductionS'.nameP,
                        Production#'ProductionS'.quant,
                        Production#'ProductionS'.value},
                    arbiter(Sock, State);

                % Send Message to GlobalState Actor with an Ack about a Order
                {m7, Ack} ->
                    State ! {ack,
                        Ack#'OrderAckS'.ack,
                        Ack#'OrderAckS'.msg,
                        Ack#'OrderAckS'.nameI},
                    arbiter(Sock, State)
            end;

    % Send Message to Arbiter Process with an Order about a Product
        {order, Msg} ->
            MsgN = #'Negotiator'{msg = {order, Msg}},
            M = nefitproto:encode_msg(MsgN),
            gen_tcp:send(Sock, M),
            arbiter(Sock, State);

    % Send Message to Arbiter Process with a new Product
        {disponibility, Msg} ->
            MsgN = #'Negotiator'{msg = {disponibility, Msg}},
            M = nefitproto:encode_msg(MsgN),
            gen_tcp:send(Sock, M),
            arbiter(Sock, State);

    % Send Message to Arbiter Process with Subscribers from a Importer
        {sub, Msg} ->
            MsgN = #'Negotiator'{msg = {sub, Msg}},
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
            Msg = nefitproto:decode_msg(Data, 'ManufacturerToServer'),
            Field = Msg#'ManufacturerToServer'.message,
            case Field of

                % Send Message to GlobalState Actor with a new Product from this Manufacturer
                {m1, ProductionOffer} ->
                    State ! {disponibility,
                        ProductionOffer#'ManufacturerToServerAnnounce'.manufacturerName,
                        ProductionOffer#'ManufacturerToServerAnnounce'.productName,
                        ProductionOffer#'ManufacturerToServerAnnounce'.minUnitPrice,
                        ProductionOffer#'ManufacturerToServerAnnounce'.minQuantity,
                        ProductionOffer#'ManufacturerToServerAnnounce'.maxQuantity,
                        ProductionOffer#'ManufacturerToServerAnnounce'.timeout}
            end,
            manufacturer(Sock, State);

    % Send Message to Manufacturer Process with a Production
        {production, Msg} ->
            MsgM = #'ServerToManufacturer'{message = {sold, Msg}},
            M = nefitproto:encode_msg(MsgM),
            gen_tcp:send(Sock, M),
            manufacturer(Sock, State);

        {noproduction, Msg} ->
            MsgM = #'ServerToManufacturer'{message = {noOffers, Msg}},
            M = nefitproto:encode_msg(MsgM),
            gen_tcp:send(Sock, M),
            manufacturer(Sock, State);

        {announced, Msg} ->
            MsgM = #'ServerToManufacturer'{message = {announced, Msg}},
            M = nefitproto:encode_msg(MsgM),
            gen_tcp:send(Sock, M),
            manufacturer(Sock, State);

    % Send Message to GlobalState Actor to be disconnect
        {tcp_closed, _} ->
            State ! {disconnectUser, self()},
            io:format("Closed.");

    % Send Message to GlobalState Actor to be disconnect
        {tcp_error, _, _} ->
            State ! {disconnectUser, self()},
            io:format("Error.")
    end.

% treats client logged as importer
importer(Sock, State) ->
    receive
        {tcp, _, Data} ->
            Msg = nefitproto:decode_msg(Data, 'ImporterToServer'),
            Field = Msg#'ImporterToServer'.message,
            case Field of

                % Send Message to GlobalState Actor with a new Order from this Importer
                {offer, Order} ->
                    State ! {order,
                        Order#'ImporterToServerOffer'.manufacturerName,
                        Order#'ImporterToServerOffer'.productName,
                        Order#'ImporterToServerOffer'.quantity,
                        Order#'ImporterToServerOffer'.unitPrice,
                        Order#'ImporterToServerOffer'.importerName},
                    importer(Sock, State);

                % Send Message to GlobalState Actor with Subscribers from this Importer
                {subscribe, Sub} ->
                    State ! {sub, Sub#'ImporterToServerSubscribe'.importerName, Sub#'ImporterToServerSubscribe'.manufacturerName},
                    importer(Sock, State)
            end;

    % Send Message to Importer Process with an Ack about an Order that he make
        {ack, Msg} ->
            MsgI = #'ServerToImporter'{message = {ordack, Msg}},
            M = nefitproto:encode_msg(MsgI),
            gen_tcp:send(Sock, M),
            importer(Sock, State);

    % Send Message to Importer Process with Result about one Product that his puts at least one Order
        {result, Msg} ->
            MsgI = #'ServerToImporter'{message = {result, Msg}},
            M = nefitproto:encode_msg(MsgI),
            gen_tcp:send(Sock, M),
            importer(Sock, State);

    % Send Message to Importer Process with Info about a new Product
        {info, Msg} ->
            MsgI = #'ServerToImporter'{message = {info, Msg}},
            M = nefitproto:encode_msg(MsgI),
            gen_tcp:send(Sock, M),
            importer(Sock, State);

    % Send Message to GlobalState Actor to be disconnect
        {tcp_closed, _} ->
            State ! {disconnectUser, self()},
            io:format("Closed.");

    % Send Message to GlobalState Actor to be disconnect
        {tcp_error, _, _} ->
            State ! {disconnectUser, self()},
            io:format("Error.")
    end.

% registers or logs in the connected client
connectedClient(Sock, State) ->
    receive
        {tcp, _, Data} ->
            Msg = nefitproto:decode_msg(Data, 'ClientToServer'),
            case Msg#'ClientToServer'.message of

                % Send Message to GlobalState Actor to Client make register
                {'ClientToServerRegister', M} ->
                    State ! {self(),
                        register,
                        M#'ClientToServerRegister'.username,
                        M#'ClientToServerRegister'.password};

                % Send Message to GlobalState Actor to Client make login
                {'ClientToServerLogin',M} ->
                    State ! {self(),
                        login,
                        M#'ClientToServerLogin'.username,
                        M#'ClientToServerLogin'.password,
                        M#'ClientToServerLogin'.clientType}
            end,
            connectedClient(Sock, State);

    % Send Message to Client Process with an Ack that login is OK
        {success, Type} ->
            Msg = #'ServerToClientAuth'{ok = true},
            M = nefitproto:encode_msg(Msg),
            gen_tcp:send(Sock, M),
            case Type of
                'IMPORTER' -> importer(Sock, State);
                'MANUFACTURER' -> manufacturer(Sock, State)
            end;

    % Send Message to Client Process with an Ack that register is OK
        {successR} ->
            Msg = #'ServerToClientAuth'{ok = true},
            M = nefitproto:encode_msg(Msg),
            gen_tcp:send(Sock, M),
            connectedClient(Sock, State);

    % Send Message to Client Process to be shutdown
        {failure} ->
            Msg = #'ServerToClientAuth'{ok = false},
            M = nefitproto:encode_msg(Msg),
            gen_tcp:send(Sock, M);

        {tcp_closed, _} ->
            io:format("Closed.");
        {tcp_error, _, _} ->
            io:format("Error.")
    end.
