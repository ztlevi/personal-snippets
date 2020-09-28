AOF stands for Append Only File. It's the change-log style persistent format.

RDB is for Redis Database File. It's the snapshot style persistence format.

## Pipelining and

Pipelining is primarily a network optimization. It essentially means the client buffers up a bunch
of commands and ships them to the server in one go. The commands are not guaranteed to be executed
in a transaction. The benefit here is saving network round trip time for every command.

Redis is single threaded so an individual command is always atomic, but two given commands from
different clients can execute in sequence, alternating between them for example.

Multi/exec, however, ensures no other clients are executing commands in between the commands in the
multi/exec sequence.
