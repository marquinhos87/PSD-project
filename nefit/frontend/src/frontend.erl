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
        {Pid, register, Name, Pass, Type} ->
            Status = maps:find(Name, RegisteredUsers),
            case Status of
                {ok, _} ->
                    Pid ! {failure},
                    globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos);
                error ->
                    Registered = maps:put(Name, Pass, RegisteredUsers),
                    Pid ! {successR},
                    Msg = #'FrontendToCatalogAddUser'{username = Name, type = Type},
                    M = nefitproto:encode_msg(Msg),
                    Str = binary:list_to_bin("addUser"),
                    chumak:send_multipart(Socket,[Str,M]),
                    globalState(Registered, ConnectedUsers, Arbiters, Socket, Pos)
            end;

    % Send Message to Arbiter Actor with a new Order from an Importer
        {order, Manuf, Product, Quant, Value, Imp} ->
            Msg = #'ServerToArbiterOffer'{
                manufacturerName = Manuf,
                productName = Product,
                quantity = Quant,
                unitPrice = Value,
                importerName = Imp},
            MsgN = #'ServerToArbiter'{message = {offer,Msg}},
            Aux = string:concat(Manuf, Product),
            Str = binary:list_to_bin(Aux),
            M = nefitproto:encode_msg(MsgN),
            chumak:send_multipart(Socket,[Str,M]),
            globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos + 1);

    % Send Message to all Arbiter Actors with the Manufacturers subscribed by an Importer
        {sub, Imp, Manuf} ->
            Msg = #'ServerToArbiterSubscribe'{
                importerName = Imp,
                manufacturerNames = Manuf},
            List = maps:to_list(Arbiters),
            [Arbiter ! {sub, Msg} || {Arbiter, _} <- List],
            globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos);

    % Send Message to an Arbiter Actor with a new Product, the Arbiter chosen where the one with less Active Negotiations
        {disponibility, M, P, Value, Min, Max, Period} ->
            Msg = #'ServerToArbiterAnnounce'{
                manufacturerName = M,
                productName = P,
                minUnitPrice = Value,
                minQuantity = Min,
                maxQuantity = Max,
                timout = Period},
            {Pid, I} = for(maps:size(Arbiters), "c", 99999999, maps:iterator(Arbiters)),
            Map = maps:put(Pid, I + 1, Arbiters),
            Pid ! {disponibility, Msg},
            globalState(RegisteredUsers, ConnectedUsers, Map, Socket, Pos);

    % Send Message to a Manufacturer Actor with the Result about one of its products
        {sold, PidA, M, P, Q, V} ->
            Msg = #'ServerToManufacturerSold'{
                productName = P,
                quantity = Q,
                unitPrice = V},
            PidU = maps:get(M, ConnectedUsers),
            PidU ! {production, Msg},
            I = maps:get(PidA, Arbiters),
            Map = maps:put(PidA, I - 1, Arbiters),
            MsgN = #'ArbiterToCatalogRemoveNegotiation'{manufacturerName = M, productName = P},
            M = nefitproto:encode_msg(MsgN),
            Str = binary:list_to_bin("removeNegotiation"),
            chumak:send_multipart(Socket,[Str,M]),
            globalState(RegisteredUsers, ConnectedUsers, Map, Socket, Pos);

        {noOffers, PidA, M, P} ->
            Msg = #'ServerToManufacturerNoOffers'{productName = P},
            PidU = maps:get(M, ConnectedUsers),
            PidU ! {noproduction, Msg},
            I = maps:get(PidA, Arbiters),
            Map = maps:put(PidA, I - 1, Arbiters),
            MsgN = #'ArbiterToCatalogRemoveNegotiation'{manufacturerName = M, productName = P},
            M = nefitproto:encode_msg(MsgN),
            Str = binary:list_to_bin("removeNegotiation"),
            chumak:send_multipart(Socket,[Str,M]),
            globalState(RegisteredUsers, ConnectedUsers, Map, Socket, Pos);

        {accepted, M, P, Min, Max, V} ->
            Msg = #'ServerToManufacturerAnnounced'{productName = P},
            PidU = maps:get(M, ConnectedUsers),
            PidU ! {announced, Msg},
            MsgN = #'ArbiterToCatalogAddNegotiation'{
                manufacturerName = M,
                productName = P,
                minQuantity = Min,
                maxQuantity = Max,
                minUnitPrice = V
            },
            M = nefitproto:encode_msg(MsgN),
            Str = binary:list_to_bin("addNegotiation"),
            chumak:send_multipart(Socket,[Str,M]),
            globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos);

        {announceInvalid, M,P, Message} ->
            Msg = #'ServerToManufacturerInvalid'{productName = P, errorMessage = Message},
            PidU = maps:get(M, ConnectedUsers),
            PidU ! {invalid, Msg},
            globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos);

        {submitted, M, P ,I} ->
            Msg = #'ServerToImporterOfferSubmitted'{manufacturerName = M, productName = P},
            PidU = maps:get(I,ConnectedUsers),
            PidU ! {submitted, Msg},
            globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos);

        {offerInvalid, M, P, I, Message} ->
            Msg = #'ServerToImporterOfferInvalid'{manufacturerName = M, productName = P, errorMessage = Message},
            PidU = maps:get(I,ConnectedUsers),
            PidU ! {offerInvalid, Msg},
            globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos);

    % Send Message to an Importer Actor with an Result about one Product that his puts at least one Order
        {won, I, M, P, Q, V} ->
            Msg = #'ServerToImporterOfferWon'{manufacturerName = M, productName = P, quantity = Q, unitPrice = V},
            Pid = maps:get(I, ConnectedUsers),
            Pid ! {won, Msg},
            globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos);

        {lose, I, M, P} ->
            Msg = #'ServerToImporterOfferLose'{
                manufacturerName = M,
                productName = P},
            Pid = maps:get(I, ConnectedUsers),
            Pid ! {lose, Msg},
            globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos);

    % Send Message to an Importer Actor with an Ack About its Order
        {offerOutdated, I, M, P} ->
            Msg = #'ServerToImporterOfferOutdated'{
                manufacturerName = M,
                productName = P},
            Pid = maps:get(I, ConnectedUsers),
            Pid ! {offerOutdated, Msg},
            globalState(RegisteredUsers, ConnectedUsers, Arbiters, Socket, Pos);

    % Send Message to an Importer Actor with Info about a Product
        {product, M, P, Min, Max, V, T, I} ->
            Msg = #'ServerToImporterNewProduct'{
                manufacturerName = M,
                productName = P,
                minQuantity = Min,
                maxQuantity = Max,
                minUnitPrice = V,
                timeout = T},
            Pid = maps:get(I, ConnectedUsers),
            Pid ! {product, Msg},
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
            Msg = nefitproto:decode_msg(Data, 'ArbiterToServer'),
            Field = Msg#'ArbiterToServer'.message,
            case Field of

                {accepted, Message} ->
                    State ! { accepted,
                        Message#'ArbiterToServerAnnounceAccepted'.manufacturerName,
                        Message#'ArbiterToServerAnnounceAccepted'.productName,
                        Message#'ArbiterToServerAnnounceAccepted'.minQuantity,
                        Message#'ArbiterToServerAnnounceAccepted'.maxQuantity,
                        Message#'ArbiterToServerAnnounceAccepted'.minUnitPrice
                    },
                    arbiter(Sock, State);

                {announceInvalid, Message} ->
                    State ! { announceInvalid,
                        Message#'ArbiterToServerAnnounceInvalid'.manufacturerName,
                        Message#'ArbiterToServerAnnounceInvalid'.productName,
                        Message#'ArbiterToServerAnnounceInvalid'.errorMessage
                    },
                    arbiter(Sock, State);

                {sold, Message} ->
                    State ! { sold, self(),
                        Message#'ArbiterToServerAnnounceSold'.manufacturerName,
                        Message#'ArbiterToServerAnnounceSold'.productName,
                        Message#'ArbiterToServerAnnounceSold'.quantity,
                        Message#'ArbiterToServerAnnounceSold'.unitPrice
                    },
                    arbiter(Sock, State);

                {noOffers, Message} ->
                    State ! {noOffers, self(),
                        Message#'ArbiterToServerAnnounceNoOffers'.manufacturerName,
                        Message#'ArbiterToServerAnnounceNoOffers'.productName
                        },
                    arbiter(Sock, State);

                {submitted, Message} ->
                    State ! {submitted,
                        Message#'ArbiterToServerOfferSubmitted'.manufacturerName,
                        Message#'ArbiterToServerOfferSubmitted'.productName,
                        Message#'ArbiterToServerOfferSubmitted'.importerName
                        },
                    arbiter(Sock, State);

                {offerInvalid, Message} ->
                    State ! {offerInvalid,
                        Message#'ArbiterToServerOfferInvalid'.manufacturerName,
                        Message#'ArbiterToServerOfferInvalid'.productName,
                        Message#'ArbiterToServerOfferInvalid'.importerName,
                        Message#'ArbiterToServerOfferInvalid'.errorMessage
                        },
                    arbiter(Sock, State);

                {won, Message} ->
                    State ! {won ,
                        Message#'ArbiterToServerOfferWon'.importerName,
                        Message#'ArbiterToServerOfferWon'.manufacturerName,
                        Message#'ArbiterToServerOfferWon'.productName,
                        Message#'ArbiterToServerOfferWon'.quantity,
                        Message#'ArbiterToServerOfferWon'.unitPrice
                        },
                    arbiter(Sock, State);

                {lose, Message} ->
                    State ! {lose,
                        Message#'ArbiterToServerOfferLose'.importerName,
                        Message#'ArbiterToServerOfferLose'.manufacturerName,
                        Message#'ArbiterToServerOfferLose'.productName
                        },
                    arbiter(Sock, State);

                {offerOutdated, Message} ->
                    State ! {offerOutdated ,
                        Message#'ArbiterToServerOfferOutdated'.importerName,
                        Message#'ArbiterToServerOfferOutdated'.manufacturerName,
                        Message#'ArbiterToServerOfferOutdated'.productName
                        },
                    arbiter(Sock, State);

                {product, Message} ->
                    State ! {product,
                        Message#'ArbiterToServerNewProduct'.manufacturerName,
                        Message#'ArbiterToServerNewProduct'.productName,
                        Message#'ArbiterToServerNewProduct'.minQuantity,
                        Message#'ArbiterToServerNewProduct'.maxQuantity,
                        Message#'ArbiterToServerNewProduct'.minUnitPrice,
                        Message#'ArbiterToServerNewProduct'.timeout,
                        Message#'ArbiterToServerNewProduct'.importerName
                        },
                    arbiter(Sock, State)
            end;

    % Send Message to Arbiter Process with an Order about a Product
        {order, Msg} ->
            MsgN = #'ServerToArbiter'{message = {offer, Msg}},
            M = nefitproto:encode_msg(MsgN),
            gen_tcp:send(Sock, M),
            arbiter(Sock, State);

    % Send Message to Arbiter Process with a new Product
        {disponibility, Msg} ->
            MsgN = #'ServerToArbiter'{message = {announce, Msg}},
            M = nefitproto:encode_msg(MsgN),
            gen_tcp:send(Sock, M),
            arbiter(Sock, State);

    % Send Message to Arbiter Process with Subscribers from a Importer
        {sub, Msg} ->
            MsgN = #'ServerToArbiter'{message = {subscribe, Msg}},
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
                {announce, ProductionOffer} ->
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

        {invalid, Msg} ->
            MsgM = #'ServerToManufacturer'{message = {invalid, Msg}},
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
                    State ! {sub,
                        Sub#'ImporterToServerSubscribe'.importerName,
                        Sub#'ImporterToServerSubscribe'.manufacturerNames},
                    importer(Sock, State)
            end;

    % Send Message to Importer Process with an Ack about an Order that he make
        {submitted, Msg} ->
            MsgI = #'ServerToImporter'{message = {offerSubmitted, Msg}},
            M = nefitproto:encode_msg(MsgI),
            gen_tcp:send(Sock, M),
            importer(Sock, State);

        {offerInvalid, Msg} ->
            MsgI = #'ServerToImporter'{message = {offerInvalid, Msg}},
            M = nefitproto:encode_msg(MsgI),
            gen_tcp:send(Sock, M),
            importer(Sock, State);

        {offerOutdated, Msg} ->
            MsgI = #'ServerToImporter'{message = {offerOutdated, Msg}},
            M = nefitproto:encode_msg(MsgI),
            gen_tcp:send(Sock, M),
            importer(Sock, State);

    % Send Message to Importer Process with Result about one Product that his puts at least one Order
        {won, Msg} ->
            MsgI = #'ServerToImporter'{message = {offerWon, Msg}},
            M = nefitproto:encode_msg(MsgI),
            gen_tcp:send(Sock, M),
            importer(Sock, State);

        {lose, Msg} ->
            MsgI = #'ServerToImporter'{message = {offerLose, Msg}},
            M = nefitproto:encode_msg(MsgI),
            gen_tcp:send(Sock, M),
            importer(Sock, State);

    % Send Message to Importer Process with Info about a new Product
        {product, Msg} ->
            MsgI = #'ServerToImporter'{message = {newProduct, Msg}},
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
%check
                % Send Message to GlobalState Actor to Client make register
                {register, M} ->
                    State ! {self(),
                        register,
                        M#'ClientToServerRegister'.username,
                        M#'ClientToServerRegister'.password,
                        M#'ClientToServerRegister'.clientType};
%check
                % Send Message to GlobalState Actor to Client make login
                {login,M} ->
                    State ! {self(),
                        login,
                        M#'ClientToServerLogin'.username,
                        M#'ClientToServerLogin'.password,
                        M#'ClientToServerLogin'.clientType}
            end,
            connectedClient(Sock, State);
%check
    % Send Message to Client Process with an Ack that login is OK
        {success, Type} ->
            Msg = #'ServerToClientAuth'{ok = true},
            M = nefitproto:encode_msg(Msg),
            gen_tcp:send(Sock, M),
            %Send Message to Catalog
            case Type of
                'IMPORTER' -> importer(Sock, State);
                'MANUFACTURER' -> manufacturer(Sock, State)
            end;
%check
    % Send Message to Client Process with an Ack that register is OK
        {successR} ->
            Msg = #'ServerToClientAuth'{ok = true},
            M = nefitproto:encode_msg(Msg),
            gen_tcp:send(Sock, M),
            connectedClient(Sock, State);
%check
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
