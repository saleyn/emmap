# Erlang MMAP `emmap`

[![build](https://github.com/saleyn/emmap/actions/workflows/erlang.yml/badge.svg)](https://github.com/saleyn/emmap/actions/workflows/erlang.yml)

This Erlang library implements an ability to use memory map files in the memory of the
Erlang virtual machine.  It offers three sets of functions to implement:

1. Generic read/write access to memory mapped files.
2. Persistent atomic integer counters supporting basic arithmetic and logical operators.
   This feature is an enhancement of the
   [Erlang's atomic counters](https://www.erlang.org/doc/man/counters.html), by adding more
   atomic operations (e.g. `xchg`, `cas`, `and`, `or`, and `xor`) for the counters, as well as
   adding counter persistence.
3. Persistent FIFO queue.
4. Persistent storage for fixed-size data blocks.

## Authors

* [Kresten Krab Thorup](https://github.com/krestenkrab/emmap)
* [Serge Aleynikov](https://github.com/saleyn/emmap)

## Supported Platforms

Linux, MacOS

NOTE: On MacOS `emmap:resize/2` is not supported, it will return `{error, fixed_size}`.

## Basic Usage

The basic usage is
```erlang
{ok, Mem, _Info} = emmap:open("filename", [read, shared, direct]),
{ok, Binary}     = file:pread(Mem, 100, 40),
...
ok = file:close(Mem).
```
The open options is a list containing zero or more [options](https://saleyn.github.io/emmap/emmap.html#type-open_option).

From this point, `Mem` can be used either with the `file` or with the `emmap` functions
interchangeably:

- `{ok, Binary} = file:pread(Mem, Position, Length)` read Length bytes at Position in the file.
- `ok = file:pwrite(Mem, Position, Binary)` writes to the given position. 
- `{ok, Binary} = file:read(Mem, Length)` read 1..Length bytes from current position, or return
  `eof` if pointer is at end of file.
- `{ok, Pos} = file:position(Mem, Where)` see file:position/2 documentation.
- `ok = file:close(Mem)`

All read/write functions invoke NIFs that don't call any IO functions but rather access memory
via calls to `memcpy(2)`, and persistence is achieved by relying on the OS implementation of
saving dirty memory pages to files.

A memory map can be closed either by calling `emmap:close/1` or `file:close/1`. When using
the `direct` option, and `emmap:close/1` is called, the memory map is not immediately closed,
but will get automatically closed when all binaries that reference this memory map are garbage
collected.

## Atomic operations on the memory mapped file

The `emmap` application offers a way to do atomic `add`, `sub`, `xchg`, `cas` as well as bitwise
`and`, `or`, `xor` operations using `emmap:patomic_*/3` and `emmap:patomic_cas/4` functions.

Effectively this directly changes the content of the underlying memory, is thread-safe, and
persistent.

```erlang
{ok, OldValue} = emmap:patomic_add(Mem, Position, 1).
```
This approach allows to implement persistent atomic counters that survive node restarts.

## Atomic persistent counters

The `emmap` application allows a user to maintain atomic persistent counters.  This could be
useful for continuous numbering of some events in the system which could be efficiently shared
among Erlang or OS processes in a thread-safe way and at the same time being persistent.
This is a very light-weight approach compared to using `mnesia` or other form of persistent
storage.

Here is an example:

```erlang
F  = emmap:open_counters("/tmp/mem.bin", 2),
N1 = emmap:inc_counter(F, 0),
N2 = emmap:inc_counter(F, 0, 5),
N3 = emmap:inc_counter(F, 0),
N4 = emmap:inc_counter(F, 0, 2),
N5 = emmap:set_counter(F, 0, 15),
N6 = emmap:read_counter(F, 0),
emmap:close_counters(F),
io_format("N1=~w, N2=~w, N3=~w, N4=~w, N5=~w, N6=~w\n",
          [N1, N2, N3, N4, N5, N6]).  % Prints: N1=0, N2=1, N3=6, N4=7, N5=9, N6=15
```

## Shared memory and using mutable binaries

While Erlang goes at length to achieve immutability, sometimes applications might need
to have access to mutable memory.  This can be accomplished by using the direct shared
access to the memory mapped file.

Example:

```erlang
shell1> {ok, MM, _Info} = emmap:open("/tmp/mem.data", 0, 8, [create, direct, read, write, shared, nolock]).
shell2> {ok, MM, _Info} = emmap:open("/tmp/mem.data", 0, 8, [create, direct, read, write, shared, nolock]).

shell1> emmap:pwrite(MM, 0, <<"test1">>).
shell2> {ok, Bin} = emmap:pread(MM, 0, 5).
{ok, <<"test1">>}

shell1> emmap:pwrite(MM, 0, <<"test2">>).
shell2> Bin.
<<"test2">>

shell1> emmap:pwrite(MM, 0, <<"test3">>).
shell2> Bin.
<<"test3">>
```
Though this may seem odd that a bound `Bin` variable returns a different value when we printed
it in the `shell2` the second time, it is the result of opening memory mapped file using the
`direct` option.  In this case the binaries read from memory map point to the actual memory
in that map rather than being copies of that memory.  For some applications, such as when
using that memory to store atomic counters, this property can be very valuable.

Using the option `direct` has the effect that the mmap file is not closed until all references
to binaries coming out of read/pread have been garbage collected.  This is a consequence of
that such binaries are referring directly to the mmap'ed memory.

When passing `auto_unlink` option to `emmap:open/4`, the memory mapped file will be
automatically deleted when it is closed.

## Shared memory without using mutable binaries

This example preserves the immutability of binaries but allows `emmap` to have visibility
of memory changes between Erlang processes and also between OS processes.

```erlang
$ erl -pa _build/default/lib/emmap/ebin
eshell#1> {ok, F, _} = emmap:open("/tmp/q.bin", 0, 128, [auto_unlink, shared, create, read,
  write]).

$ erl -pa _build/default/lib/emmap/ebin
eshell#2> {ok, F, _} = emmap:open("/tmp/q.bin", 0, 128, [auto_unlink, shared, create, read,
  write]).

eshell#1> emmap:pwrite(F, 0, <<"abcdefg\n">>).

eshell#2> emmap:pread(F, 0, 8). % Changes in eshell#1 are visible in eshell#2
{ok, <<"abcdefg\n">>}

$ head -1 /tmp/q.bin            # They are also visible in another OS process reading from file
abcdefg
```

Here it is without the `shared` option:
```erlang
$ erl -pa _build/default/lib/emmap/ebin
eshell#1> emmap:close(F).
eshell#1> f(F), {ok, F, _} = emmap:open("/tmp/q.bin", 0, 128, [auto_unlink, create, read,
  write]).

^G
--> s        % Start a new shell process inside the same Erlang VM
--> c 2      % Connect to the new shell
eshell#2> f(F), {ok, F, _} = emmap:open("/tmp/q.bin", 0, 128, [auto_unlink, create, read,
  write]).

^G
--> c 1      % Switch back to the 1st shell
eshell#1> emmap:pwrite(F, 0, <<"1234567\n">>).

^G
--> c 2      % Switch to the 2st shell

eshell#2> emmap:pread(F, 0, 8).
{ok,<<0,0,0,0,0,0,0,0>>}    % changes from shell1 are invisible in the shell2 Erlang process

# Run this in another terminal
$ head -1 /tmp/q.bin        # returns no data because changes in shell1 are invisible
```

## Persistent FIFO used as a container or guarded by a gen_server process

The `emmap_queue` module implements a persistent FIFO queue based on a memory-mapped file.
This means that in-memory operations of enqueuing items are automatically persisted on disk.

A queue used as a container will persistent messages stored in queue on disk, and has constant
time complexity of the push and pop operations.  The `open/3` is given an initial storage in
bytes, which will automatically grow unless the `fixed_size` option is provided, in which case
when the queue becomes full, a `push/2` call will return `{error, full}`.  In the example below
we are using `auto_unlink` option which automatically deletes the memory mapped file at the end
of the test case (something you might not want in other cases):

```erlang
{ok, Q} = emmap_queue:open(Filename, 1024, [auto_unlink]),
ok = emmap_queue:push(Q, a),
ok = emmap_queue:push(Q, {b,1}),
ok = emmap_queue:push(Q, {c,d}),

a     = emmap_queue:pop(Q),
{b,1} = emmap_queue:pop(Q),
{c,d} = emmap_queue:pop(Q),
nil   = emmap_queue:pop_and_purge(Q).
```

Use `emmap_queue:pop_and_purge/1` to reclaim the space in memory when the queue becomes empty.

When a queue is wrapped in a `gen_server`, it is suitable for use in a multi-process use cases.
This is implemented using `emmap_queue:start_link/4`,`emmap_queue:enqueue/2`, and
`emmap_queue:dequeue/1` functions.  In the example below we are using the `auto_unlink` option
which automatically deletes the memory mapped file at the end of the test case (something you
might not want in other cases):


```erlang
{ok, Pid} = emmap_queue:start_link(?MODULE, Filename, 1024, [auto_unlink]),
ok = emmap_queue:enqueue(Pid, a),
ok = emmap_queue:enqueue(Pid, {b,1}),
ok = emmap_queue:enqueue(Pid, {c,d}),

a     = emmap_queue:dequeue(Pid),
{b,1} = emmap_queue:dequeue(Pid),
{c,d} = emmap_queue:dequeue(Pid),
nil   = emmap_queue:dequeue(Pid).
```

## Persistent storage for fixed-size data blocks

The purpose is to store, read, and remove arbitrary data blocks of a fixed size. Each block of
data stored has a unique integer address (internally translated into an offset from the beginning
of the memory-mapped file). Persistent storage tries to reuse a free block closest to the file start
or allocate a new block when needed. The file may be automatically resized (expanded) when required.

To start using a memory-mapped file as a storage call `emmap:init_block_storage/2` providing emmap
handler and block size (it will be saved in the storage header).

The new flag `fit` added to emmap:open/4 option list. When set and the existing file opened has a size
less than requested region length, the file will be stretched to the given length. If the file size is
greater than requested, with fit flag the mapped region will fit the file size. Without the `fit` flag
attempt to map existing file of a different size will result in error.

```erlang
  % open underlying memory-mapped file
  {ok, MFile, Info} = emmap:open("storage.bin", 0, 4096, [create, write, fit, shared]),

  % init block storage of the fixed block size
  ok = emmap:init_block_storage(MFile, 22),
```

When opening an existing file, it may be possible that it was left in an inconsistent state in
case of abnormal termination of the program modifying it. To ensure consistency, call
`emmap:repair_block_storage/1` to check and fix the file at once, or repeatedly call
`emmap:repair_block_storage/3`. The latter version with continuation is recommended for relatively
big storages, to avoid long-running NIF calls. The repair operation checks (and fixes)
inconsistency between "free blocks" and "used blocks" masks in the internal tree-like representation.

```erlang
repair_chunks(MFile, N) ->
  repair_chunks(MFile, 0, N).

repair_chunks(_MFile, eof, _) ->
  ok;
repair_chunks(MFile, Start, N) ->
  Cont = emmap:repair_block_storage(MFile, Start, N),
  repair_chunks(MFile, Cont, N).
```

To read all blocks, use `emmap:read_blocks/1`, or `emmap:read_blocks/3` for reads with continuation,
limiting the number of blocks read in one shot, to avoid long-running NIF calls.

```erlang
  List = emmap:read_blocks(MFile),
```
or
```erlang
read_chunks(MFile, N) ->
  read_chunks(MFile, 0, N, []).

read_chunks(_MFile, eof, _, Acc) ->
  lists:concat(Acc);
read_chunks(MFile, Start, N, Acc) ->
  {L, Cont} = emmap:read_blocks(MFile, Start, N),
  read_chunks(MFile, Cont, N, [L | Acc]).
```

The function `emmap:read_block/2` reads the block at the given address, `emmap:store_block/2`
writes the data block into the storage, and `emmap:free_block/2` deletes the block at the given
address.

```erlang
  Addr = emmap:store_block(MFile, Data),
  Bytes = emmap:read_block(MFile, Addr),
  emmap:free_block(MFile, Addr),
```

The storage capacity is limited by internal organization. It depends on the number of tree levels, and can be
configured by defining the environment variable `BS_LEVELS`. The maximum number of stored blocks is `64 ^ BS_LEVELS`.
The default `BS_LEVELS` value is 3, so the default capacity is `64 * 64 * 64 = 262144` blocks.

An attempt to store a block will return `{error, full}` when the storage has no free slots.

The result of freeing a block is `true` on success, `false` if there is no block with the given address
found, or `{error, Reason}` for common emmap error cases.

The `read_block/2` returns bytes, `eof` when no block exists at the given address or common error.
