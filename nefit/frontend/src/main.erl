
-module(main).
-export([server/1]).

-include("nefit.hrl").

server(Port) ->
    Authenticator = spawn(fun()-> authenticator(map:new()) end),
    Orders = spawn(fun()-> receiveOrder([]) end),
    Productions = spawn(fun()-> receiveProduction([],[]) end),
    Arbiters = spawn(fun()-> arbiterResults(map:new()) end),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),
    acceptor(LSock, Authenticator).

% accepts connections
acceptor(LSock, Authenticator) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, Authenticator) end),
    connectedClient(Sock, Authenticator).

% process responsible for article orders
receiveOrder(Orders) ->
    receive
        {Order, Details} ->
            % send the order to the arbiter with the least amount of load
            Details
    end.

% process responsible for article productions
receiveProduction(Productions, Subs) ->
    Productions.

% process responsible for the results received by the arbiters
arbiterResults(Arbiters) ->
    % initialize the number of arbiters
    receive
        {tcp, _, Data} ->
            % received the result of a negotiation
            decoder(Data)
    end.

% treats client logged as manufacturer
manufacturer(Sock) ->
    receive
        {tcp, _, Data} ->
            decoder(Data),
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
            decoder(Data),
            importer(Sock);
        {tcp_closed, _} ->
            io:format("Closed.");
        {tcp_error, _, _} ->
            io:format("Error.")
    end.

% registers or logs in the connected client
connectedClient(Sock, Authenticator) ->
    receive
        {tcp, _, Data} ->
            Msg = decoder(Data,'MsgAuth'),
            Authenticator ! {Msg#'MsgAuth'.mtype, Msg#'MsgAuth'.name, Msg#'MsgAuth'.pass, Msg#'MsgAuth'.ctype,Sock},
            connectedClient(Sock, Authenticator);
        {success, Type} ->
            case Type of
                'IMPORTER'-> importer(Sock);
                'MANUFACTURER'-> manufacturer(Sock)
            end;
        {tcp_closed, _} ->
            io:format("Closed.");
        {tcp_error, _, _} ->
            io:format("Error.")
    end.

% process responsible for authenticating and maybe register users, might separate
authenticator(RegisteredUsers) ->
    receive
        {'LOGIN', Name, Pass, Type, Sock} ->
            Status = maps:find(Name,RegisteredUsers),
            case Status of
                {ok, Value} ->
                    if
                        Value == Pass ->
                            Sock ! {success, Type};
                        true ->
                            Sock ! {failure}
                    end;
                error ->
                    Sock ! {failure}
            end;
        {'REGISTER', Name, Pass, Type, Sock} ->
            Status = maps:find(Name,RegisteredUsers),
            case Status of
                {ok, _} ->
                    Sock ! {failure},
                    authenticator(RegisteredUsers);
                error ->
                    Registered = maps:put(Name, Pass, RegisteredUsers),
                    authenticator(Registered)
            end
    end.

% serialize


% deserialize
decoder(Data) ->
    Details = nefit:decode_msg(Data, 'Server'),
    Details.

decoder(Data, Type) ->
    Details = nefit:decode_msg(Data, Type),
    Details.