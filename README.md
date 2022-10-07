# proof-of-work-cluster
Erlang modules to create a cluster of actors which can find tokens which generate hashes with a particular number of zeros.

## supervisor.erl
`c(supervisor).`

`Prefix2Length = 4.`

`StepSize = 100000.`

`NumberOfZeros = 3.`

`Pid = supervisorActor:init('nicolasgarcia', Prefix2Length, StepSize, NumberOfZeros).`

`Pid = supervisorActor:init("nicolasgarcia", 4, 100000, 3).`

`erlang:process_info(list_to_pid("<0.88.0>"), messages).`

## worker.erl
`SPid = list_to_pid("<0.87.0>").`

`c(worker).`

`WPid = worker:init(list_to_pid("<0.93.0>")).`

`WPid = worker:init(SPid).`

`wk1 ! {start, list_to_pid("<0.93.0>")}.`

`wk1 ! {start, SPid}.`

## Set Cookie
### If using two or more nodes on a local network:
`erl -name anurag@10.20.242.14 -setcookie xyz`

The name should have an identifier and a private IP address of the node.
In the above example, the identifier is anurag, and the private IP is `10.20.242.14`.

### If using a single node:
`erl -name a@127.0.0.1 -setcookie xyz`

`erl -name b@127.0.0.1 -setcookie xyz`

### Connect the nodes
`net_adm:ping('b@127.0.0.1').`

## Other commands
pid function creates a process id from a set of numbers.

`P = pid(6075,50,0).`

rpc call method lists the set of processes on another node.

`rpc:call('a@127.0.0.1',erlang,processes,[]).`


## How to use
### Terminal A
1. Open a new terminal (let's refer to as Terminal A), change to `src` directory and run `erl -name a@127.0.0.1 -setcookie xyz`.
2. Compile the supervisor using `c(supervisorActor).`.
3. Start the supervisor using `supervisorActor:init("anurags", 4, 10000, 2).`
4. Connect the two nodes by running `net_adm:ping('b@127.0.0.1').` in terminal A.

### Terminal B
1. Open a new terminal (Terminal B), change to `src` directory and run `erl -name b@127.0.0.1 -setcookie xyz`.
2. Compile the worker code using `c(worker).`.
3. Start the worker `worker:init(server).`.
4. Save the Pid of the worker, printed in previous step's output in a variable `W = pid(0,99,0).`
5. Start the mining by sending `{start}` message to the worker using `W ! {start}.`.

Now you should see the worker mining for tokens with the given number of zeros in its hash.

You should also see the tokens with the required number of zeros showing up in Terminal A.
