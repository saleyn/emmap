# Erlang MMAP `emmap`

[![build](https://github.com/saleyn/emmap/actions/workflows/erlang.yml/badge.svg)](https://github.com/saleyn/emmap/actions/workflows/erlang.yml)

This Erlang library provides a wrapper that allows you to memory map files into the Erlang memory space.  

## Authors

* [Kresten Krab Thorup](https://github.com/krestenkrab/emmap)
* [Serge Aleynikov](https://github.com/saleyn/emmap)

## Basic Usage

The basic usage is
```erlang
{ok, Mem} = emmap:open("filename", [read, shared, direct]),
{ok, Binary} = file:pread(Mem, 100, 40),
...
ok = file:close(Mem).
```
The open options is a list containing zero or more [options](https://saleyn.github.io/emmap/emmap.html#type-open_option).

From this point, `Mem` can be used with the `file` operations:

- `{ok, Binary} = file:pread(Mem, Position, Length)` read Length bytes at Position in the file.
- `ok = file:pwrite(Mem, Position, Binary)` writes to the given position. 
- `{ok, Binary} = file:read(Mem, Length)` read 1..Length bytes from current position, or return `eof` if pointer is at end of file.
- `{ok, Pos} = file:position(Mem, Where)` see file:position/2 documentation.
- `ok = file:close(Mem)`

Additionally, the read/write operations can be performed by the corresponding functions in the `emmap` module.

## Atomic operations on the memory mapped file

The `emmap` application offers a way to do atomic ADD, SUB, XCHG as well as bitwise
AND, OR, XOR operations using `emmap:patomic/4` function.

Effectively this directly changes the content of the underlying memory and is thread-safe.

```erlang
{ok, OldValue} = emmap:patomic(Mem, Position, add, 1).
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
shell1> {ok, MM} = emmap:open("/tmp/mem.data", 0, 8, [create, direct, read, write, shared, nolock]).
shell2> {ok, MM} = emmap:open("/tmp/mem.data", 0, 8, [create, direct, read, write, shared, nolock]).

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

## Notes

Using the option `direct` has the effect that the mmap file is not closed until all references
to binaries coming out of read/pread have been garbage collected.  This is a consequence of
that such binaries are referring directly to the mmap'ed memory.  
