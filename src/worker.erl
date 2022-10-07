-module(worker).
-import(crypto, [hash/2]).
-import(rng, [rnd_number/1]).

-export([init/1, hashActor/1, hashAndCheck/2, getSha256Hash/1, countZeros/2]).

% 94 possible chars
numberToString(N) when N < 94 ->
    % 33 = '!' 33 + 93 = 126 = '~' last acceptable char to us
    [(N + 33)];
numberToString(N) when N >= 94 ->
    numberToString(N div 94) ++ numberToString(N rem 94).

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

prepareString(Prefix1, Curr) ->
    ToHash = Prefix1 ++ numberToString(Curr),
    ToHash.

findToken(_, _, _, High, Curr, _) when Curr > High ->
    ok;
findToken(SupervisorPid, Prefix, Low, High, Curr, NumberOfZeros) ->
    % ToHash = generateString(Prefix, Curr, LengthOfString),
    ToHash = prepareString(Prefix, Curr),
    HashResult = hashAndCheck(ToHash, NumberOfZeros),
    io:format("String: ~p, Hash: ~p~n", [ToHash, getSha256Hash(ToHash)]),
    if
        HashResult == true ->
            Hash = getSha256Hash(ToHash),
            SupervisorPid ! {found, self(), ToHash, Hash},
            findToken(SupervisorPid, Prefix, Low, High, Curr + 1, NumberOfZeros);
        true ->
            findToken(SupervisorPid, Prefix, Low, High, Curr + 1, NumberOfZeros)
    end.

hashActor(SupervisorPid) ->
    receive
        {health_check} ->
            io:format("Running health check~n", []),
            SupervisorPid ! {health_check, self()},
            hashActor(SupervisorPid);
        {health_response, M, _} ->
            io:format("~p~n", [M]),
            hashActor(SupervisorPid);
        {start} ->
            io:format("Starting the mining process by requesting supervisor for work~n", []),
            io:format("Supervisor Pid: ~p~n", [SupervisorPid]),
            SupervisorPid ! {start, self()},
            hashActor(SupervisorPid);
        {mine, _, Prefix1, Low, High, NumberOfZeros} ->
            io:format("Starting the mining process for values between ~p and ~p.~n", [Low, High]),
            findToken(SupervisorPid, Prefix1, Low, High, Low, NumberOfZeros),
            hashActor(SupervisorPid)
    end.

findSupervisorPid(SupervisorName) ->
    global:send(SupervisorName, {health_check, self()}),
    R =
        receive
            {health_response, M, Pid} ->
                io:format("~p~n", [M]),
                [ok, M, Pid]
        after 5000 -> {error, "no answer!"}
        end,
    lists:nth(3, R).

% messageSupervisor(SupervisorName, WorkerPid) ->
%     io:format("Starting the mining process by requesting supervisor for work~n", []),
%     io:format("Supervisor Name: ~p~n", [SupervisorName]),
%     global:send(SupervisorName, {start, WorkerPid}).
% % SupervisorPid ! {start, self()}.

init(SupervisorName) ->
    SupervisorPid = findSupervisorPid(SupervisorName),
    io:format("~p~n", [SupervisorPid]),
    % Pid = spawn(worker, hashActor, [spid]),
    Pid = spawn(fun() -> hashActor(SupervisorPid) end),
    % we keep track of the process id
    global:register_name(wk1, Pid),
    % messageSupervisor(server, Pid),

    io:format("~p~n", [Pid]),
    % Pid ! {start, SupervisorPid},
    io:format("~p\n", [wk1]),
    ok.
