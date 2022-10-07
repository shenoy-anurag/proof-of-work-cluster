-module(supervisorActor).

-import(rng, [rnd_chars/1]).

-export([init/4, superVisorListener/5]).

superVisorListener(Prefix1, Prefix2Length, Curr, StepSize, NumberOfZeros) ->
    receive
        {health_check, WorkerPid} ->
            io:format("Worker: ~p~n", [WorkerPid]),
            WorkerPid ! {health_response, "successful health check", self()},
            superVisorListener(Prefix1, Prefix2Length, Curr, StepSize, NumberOfZeros);
        {start, WorkerPid} ->
            % Prefix2 = rng:rnd_chars(Prefix2Length),
            % io:format("Prefix: ~p~n", [Prefix2]),
            High = Curr + StepSize,
            io:format(
                "Telling worker ~p to start mining tokens between ~p and ~p for a hash.~n",
                [WorkerPid, Curr, High]
            ),
            WorkerPid ! {mine, server, Prefix1, Curr, High, NumberOfZeros},
            superVisorListener(Prefix1, Prefix2Length, Curr + StepSize, StepSize, NumberOfZeros);
        {found, WorkerPid, Token, Hash} ->
            io:format("Worker ~p found token! Token: ~p, Hash: ~p~n", [WorkerPid, Token, Hash]),
            superVisorListener(Prefix1, Prefix2Length, Curr, StepSize, NumberOfZeros)
        % {not_found, WorkerPid, _, _} ->
        %     Prefix2 = rng:rnd_chars(Prefix2Length),
        %     High = Curr + StepSize,
        %     io:format(
        %         "Worker ~p couldn't find token! Mining for tokens between ~p and ~p for a hash with ~p prefix.~n",
        %         [WorkerPid, Curr, High, Prefix2]
        %     ),
        %     WorkerPid ! {mine, self(), Prefix1, Curr, High, NumberOfZeros},
        %     superVisorListener(Prefix1, Prefix2Length, Curr, StepSize, NumberOfZeros)
    end.

init(Prefix1, Prefix2Length, StepSize, NumberOfZeros) ->
    % Pid = spawn(supervisor, superVisorListener, [Prefix1, Prefix2Length, 0, StepSize, NumberOfZeros]),
    Pid = spawn(fun() -> superVisorListener(Prefix1, Prefix2Length, 0, StepSize, NumberOfZeros) end),
    % we keep track of the process id
    global:register_name(server, Pid),
    % register(spid, Pid),
    % io:format("~p\n", [spid]),
    io:format("~p~n", [Pid]),
    ok.
