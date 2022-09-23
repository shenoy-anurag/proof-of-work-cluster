-module(worker).
-import(crypto, [hash/2]).
-import(rng, [rnd_number/1]).

-export([init/0, hashActor/0, hashAndCheck/2, getSha256Hash/1, countZeros/2]).

getSha256Hash(N) ->
    HashInteger =
        binary:decode_unsigned(
            crypto:hash(
                sha256,
                N
            )
        ),
    io_lib:format("~64.16.0b", [HashInteger]).

countZeros(_, 0) ->
    true;
countZeros(Hash, NumberOfZeros) ->
    if
        % hd(Hash) == hd([0]) ->
        hd(Hash) == hd("0") ->
            countZeros(tl(Hash), NumberOfZeros - 1);
        true ->
            false
    end.

hashAndCheck(ToHash, NumberOfZeros) ->
    Hash = getSha256Hash(ToHash),
    % io:format("sha256 hash: ~p~n", [Hash]),
    Result = countZeros(Hash, NumberOfZeros),
    if
        Result == true ->
            true;
        true ->
            false
    end.

generateString(Prefix1, Prefix2, LengthOfString) ->
    RndStr = rng:rnd_number(LengthOfString),
    ToHash = Prefix1 ++ Prefix2 ++ RndStr,
    ToHash.

findToken(Prefix, Low, High, Curr, LengthOfString, NumberOfZeros) ->
    ToHash = generateString(Prefix, Curr, LengthOfString),
    HashResult = hashAndCheck(ToHash, NumberOfZeros),
    if
        HashResult == true ->
            [true, ToHash];
        
        true ->
            [false, none]
    end,
    findToken(Prefix, Low, High, Curr - 1, LengthOfString, NumberOfZeros).

hashActor() ->
    receive
        {Client, ToHash, NumberOfZeros} ->
            Hash = getSha256Hash(ToHash),
            io:format("sha256 hash: ~p~n", [Hash]),
            Result = countZeros(Hash, NumberOfZeros),
            if
                Result == true ->
                    Client ! {Hash, NumberOfZeros};
                %% else case or default
                true ->
                    Client ! {"No leading Zero", Hash}
            end;
        {Client, _} ->
            Client ! "Missing 1 parameter";
        {Client} ->
            Client ! "Missing 2 parameters"
    end,
    hashActor().

init() ->
    Pid = spawn(worker, hashActor, []),
    % we keep track of the process id
    register(wk1, Pid),
    io:format("~p~n", [Pid]),
    io:format("~p\n", [wk1]).
