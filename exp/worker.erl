-module(worker).
-import(crypto, [hash/2]).
-import(rng, [rnd_number/1]).

-export([printShaHash/1, getSha256Hash/1]).

printShaHash(N) ->
    io_lib:format("~64.16.0b", [
        binary:decode_unsigned(
            crypto:hash(
                sha256,
                N
            )
        )
    ]).

getSha256Hash(N) ->
    HashInteger =
        binary:decode_unsigned(
            crypto:hash(
                sha256,
                N
            )
        ),
    % HashInteger.
    io_lib:format("~64.16.0b", [HashInteger]).
    % L = integer_to_list(HashInteger, 16),
    % L.
    % Hash = io_lib:format("~64.16.0b", [
    %     binary:decode_unsigned(
    %         crypto:hash(
    %             sha256,
    %             N
    %         )
    %     )
    % ]),
    % Hash.

% debugCountZeros(Hash, NumberOfZeros) ->
%     io:format("~p~n", [Hash]),
%     io:format("~p~n", [hd([0])]),
%     io:format("~p~n", [NumberOfZeros]),
%     if
%         hd(Hash) == hd([0]) ->
%             io:format("Hash starts with a Zero!", []);
%         true ->
%             io:format("Hash does not start with a Zero :(", [])
%     end,
%     ok.