# Erlang MMAP `emmap`

This Erlang library provides a wrapper that allows you to memory map files into the Erlang memory space.  


## Basic Usage

The basic usage is

    {ok, Mem} = emmap:open("filename", [read, shared, direct]),
    {ok, Binary} = file:pread(Mem, 100, 40),
    ...
    ok = file:close(Mem).

The open options is a list containing zero or more of these:

- `read`, `write`: Open for reading and/or writing (you can specify both).
- `private`, `shared`: The file is opened with copy-on-write semantics, or sharing memory with the underlying file.
- `noreserve`: Do not reserve swap space for this mapping.
- `populate`: Populate (prefault) page tables for a mapping.  For a file mapping, this causes read-ahead on the file.  Later accesses to the mapping will not be blocked by page faults.
- `direct`: read/pread operations do not copy memory, but rather use "resource binaries" that can change content if the underlying data is changed.  This is the most performant, but also has other thread-safety implications.
- `lock`, `nolock` do (or do not) use a semaphore to control state changes internally in the NIF library.  
- `auto_unlink` automatically deletes the mapped file after the mapped data was garbage collected. This can be used when the mapped file is a file-based shared-memory area (e.g. `/run/shm/...`) and is mapped in `direct` mode to free the memory after the data was gc'd.
- `{address, integer()}`, `fixed`: Open mapping at the given memory address (sets MAP_FIXED on the memory mapped file).

From this point, `Mem` can be used with the `file` operations

- `{ok, Binary} = file:pread(Mem, Position, Length)` read Length bytes at Position in the file.
- `ok = file:pwrite(Mem, Position, Binary)` writes to the given position. 
- `{ok, Binary} = file:read(Mem, Length)` read 1..Length bytes from current position, or return `eof` if pointer is at end of file.
- `{ok, Pos} = file:position(Mem, Where)` see file:position/2 documentation.
- `ok = file:close(Mem)`

## Atomic operations on the memory mapped file

The `emmap` application offers a way to do atomic ADD, SUB, XCHG as well as bitwise AND, OR, XOR operations using `emmap:patomic/4` function.

Effectively this directly changes the content of the underlying memory and is thread-safe.

```erlang
{ok, OldValue} = emmap:patomic(Mem, Position, add, 1).
```
This approach allows to implement persistent atomic counters that survive node restarts.

## Notes

Using the option `direct` has the effect that the mmap file is not closed until all references to binaries coming out of read/pread have been garbage collected.  This is a consequence of that such binaries are referring directly to the mmap'ed memory.  

