-module(supervisorActor).

-import(rng, [rnd_chars/1]).

-export([init/4, superVisorListener/5]).

superVisorListener(Prefix1, Prefix2Length, Curr, StepSize, NumberOfZeros) ->
    receive
        {start, WorkerPid} ->
            Prefix2 = rng:rnd_chars(Prefix2Length),
            High = Curr + StepSize,
            io:format(
                "Telling worker ~p to start mining tokens between ~p and ~p for a hash with ~p prefix.~n",
                [WorkerPid, Curr, High, Prefix2]
            ),
            WorkerPid ! {mine, spid, Prefix1, Prefix2, Curr, High, NumberOfZeros};
        {found, WorkerPid, Token, Hash} ->
            io:format("Worker ~p found token! Token: ~p, Hash: ~p~n", [WorkerPid, Token, Hash]);
        {not_found, WorkerPid, _, _} ->
            Prefix2 = rng:rnd_chars(Prefix2Length),
            High = Curr + StepSize,
            io:format(
                "Worker ~p couldn't find token! Mining for tokens between ~p and ~p for a hash with ~p prefix.~n",
                [WorkerPid, Curr, High, Prefix2]
            ),
            WorkerPid ! {mine, self(), Prefix1, Prefix2, Curr, High, NumberOfZeros}
    end,
    superVisorListener(Prefix1, Prefix2Length, Curr, StepSize, NumberOfZeros).

init(Prefix1, Prefix2Length, StepSize, NumberOfZeros) ->
    % Pid = spawn(supervisor, superVisorListener, [Prefix1, Prefix2Length, 0, StepSize, NumberOfZeros]),
    Pid = spawn(fun() -> superVisorListener(Prefix1, Prefix2Length, 0, StepSize, NumberOfZeros) end),
    % we keep track of the process id
    register(spid, Pid),
    io:format("~p~n", [Pid]),
    io:format("~p\n", [spid]),
    Pid.
