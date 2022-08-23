# Erlang MMAP `emmap`

[![build](https://github.com/saleyn/emmap/actions/workflows/erlang.yml/badge.svg)](https://github.com/saleyn/emmap/actions/workflows/erlang.yml)

This Erlang library provides a wrapper that allows you to memory map files into the Erlang memory space.  

## Authors

* [Kresten Krab Thorup](https://github.com/krestenkrab/emmap)
* [Serge Aleynikov](https://github.com/saleyn/emmap)

## Basic Usage

The basic usage is
```erlang
{ok, Mem, _Info} = emmap:open("filename", [read, shared, direct]),
{ok, Binary}     = file:pread(Mem, 100, 40),
...
ok = file:close(Mem).
```
The open options is a list containing zero or more [options](https://saleyn.github.io/emmap/emmap.html#type-open_option).

A memory map can be closed either by calling `emmap:close/1` or `file:close/1`.

From this point, `Mem` can be used either with the `file` operations:

- `{ok, Binary} = file:pread(Mem, Position, Length)` read Length bytes at Position in the file.
- `ok = file:pwrite(Mem, Position, Binary)` writes to the given position. 
- `{ok, Binary} = file:read(Mem, Length)` read 1..Length bytes from current position, or return `eof` if pointer is at end of file.
- `{ok, Pos} = file:position(Mem, Where)` see file:position/2 documentation.
- `ok = file:close(Mem)`

Additionally, the read/write operations can be performed by the corresponding functions in the `emmap` module.

## Atomic operations on the memory mapped file

The `emmap` application offers a way to do atomic `add`, `sub`, `xchg`, `cas` as well as bitwise
`and`, `or`, `xor` operations using `emmap:patomic_*/3` and `emmap:patomic_cas/4` functions.

Effectively this directly changes the content of the underlying memory and is thread-safe.

```erlang
{ok, OldValue} = emmap:patomic_add(Mem, Position, 1).
```
This approach allows to implement persistent atomic counters that survive node restarts.

## Atomic persistent counters

The `emmap` application allows a user to maintain atomic persistent counters.  This could be
useful for continuous numbering of some events in the system which could be efficiently shared
among processes in a thread-safe way and at the same time being persistent. This is a very
light-weight approach compared to using `mnesia` or other form of persistent storage.

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

## Persistent FIFO single-process and multi-producer-single-consumer queues.

The `emmap_queue` module implements a persistent FIFO queue based on a memory-mapped file.
This means that in-memory operations of enqueuing items are automatically persisted on disk.

A single-process queue is used in a single process where the queue is used as a persistent
container of messages.  The `open/3` is given an initial storage in bytes, which will
automatically grow unless the `fixed_size` option is provided, in which case when the queue
becomes full, a `push/2` call will return `{error, full}`.  In this example we are using
`auto_unlink` option which automatically deletes the memory mapped file at the end of the
test case (something you might not want in other cases):

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

When a queue is wrapped in a `gen_server`, it is suitable for use in a
multi-producer-single-consumer use case.  This is implemented using `emmap_queue:start_link/4`,
`emmap_queue:enqueue/2`, and `emmap_queue:dequeue/1` functions.  In this example we are using
`auto_unlink` option which automatically deletes the memory mapped file at the end of the
test case (something you might not want in other cases):


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
