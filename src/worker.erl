-module(worker).
-import(crypto, [hash/2]).
-import(rng, [rnd_number/1]).

-export([init/1, hashActor/1, hashAndCheck/2, getSha256Hash/1, countZeros/2]).

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

% generateString(Prefix1, Prefix2, LengthOfString) ->
%     RndStr = rng:rnd_number(LengthOfString),
%     ToHash = Prefix1 ++ Prefix2 ++ RndStr,
%     ToHash.

prepareString(Prefix1, Prefix2, Curr) ->
    ToHash = Prefix1 ++ Prefix2 ++ Curr,
    ToHash.

findToken(Prefix, RndmPrefix, Low, High, Curr, NumberOfZeros) ->
    % ToHash = generateString(Prefix, Curr, LengthOfString),
    ToHash = prepareString(Prefix, RndmPrefix, Curr),
    HashResult = hashAndCheck(ToHash, NumberOfZeros),
    if
        Curr > High ->
            [false, none, none];
        true ->
            [ok, none, none]
    end,
    if
        HashResult == true ->
            Hash = getSha256Hash(ToHash),
            [true, ToHash, Hash];
        true ->
            [false, none, none]
    end,
    findToken(Prefix, RndmPrefix, Low, High, Curr + 1, NumberOfZeros).

hashActor(SupervisorPid) ->
    receive
        % {} ->
        %     io:format("Looping~p~n", [SupervisorPid]);
        {start, SupervisorPid} ->
            io:format("Starting the mining process by requesting supervisor for work~n", []),
            io:format("Supervisor Pid: ~p~n", [SupervisorPid]),
            SupervisorPid ! {start, self()};
        {mine, SupervisorPid, Prefix1, Prefix2, Low, High, NumberOfZeros} ->
            Output = findToken(Prefix1, Prefix2, Low, High, Low, NumberOfZeros),
            Result = hd(Output),
            ToHash = lists:nth(2, Output),
            Hash = lists:nth(3, Output),
            if
                Result == true ->
                    SupervisorPid ! {found, self(), ToHash, Hash};
                %% else case or default
                true ->
                    SupervisorPid ! {not_found, self(), none, none}
            end
    end,
    hashActor(SupervisorPid).

init(SupervisorPid) ->
    % Pid = spawn(worker, hashActor, [spid]),
    Pid = spawn(fun() -> hashActor(SupervisorPid) end),
    % we keep track of the process id
    register(wk1, Pid),
    io:format("~p~n", [Pid]),
    % Pid ! {start, SupervisorPid},
    io:format("~p\n", [wk1]),
    Pid.
