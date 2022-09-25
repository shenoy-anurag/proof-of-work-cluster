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

`net_adm:ping('b@127.0.0.1').`
