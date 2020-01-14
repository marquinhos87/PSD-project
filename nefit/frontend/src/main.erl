
-module(main).
-export([server/1]).

server(Port) ->
    Authenticator = spawn(fun()-> authenticator(map:new()) end),
    Orders = spawn(fun()-> receiveOrder([]) end),
    Productions = spawn(fun()-> receiveProduction([]) end),
    Arbiters = spawn(fun()-> arbiterResults(map:new()) end),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),
    acceptor(LSock).

% accepts connections
acceptor(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock) end),
    connectedClient(Sock).

% process responsible for article orders
receiveOrder(Orders) ->
    receive
        {Order, Details} ->
            % send the order to the arbiter with the least amount of load
            Details
    end.

% process responsible for article productions
receiveProduction(Productions) ->
    Productions.

% process responsible for the results received by the arbiters
arbiterResults(Arbiters) ->
    % initialize the number of arbiters
    receive
        {tcp, _, Data} ->
            % received the result of a negotiation
            decode(Data)
    end.

% treats client logged as manufacturer
manufacturer(Sock) ->
    receive
        {tcp, _, Data} ->
            decode(Data),
            manufacturer(Sock);
        {tcp_closed, _} ->
            io:format("Closed.");
        {tcp_error, _, _} ->
            io:format("Error.")
    end.

% treats client logged as importer
importer(Sock) ->
    receive
        {tcp, _, Data} ->
            decode(Data),
            importer(Sock);
        {tcp_closed, _} ->
            io:format("Closed.");
        {tcp_error, _, _} ->
            io:format("Error.")
    end.

% registers or logs in the connected client
connectedClient(Sock) ->
    Sock.

% process responsible for authenticating and maybe register users, might separate
authenticator(RegisteredUsers) ->
    RegisteredUsers.

% serialize
encode(Data) ->
    Data.
% deserialize
decode(Data) ->
    Data.