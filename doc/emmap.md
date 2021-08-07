

# Module emmap #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-mmap_file">mmap_file()</a> ###


<pre><code>
mmap_file() = #file_descriptor{}
</code></pre>




### <a name="type-open_option">open_option()</a> ###


<pre><code>
open_option() = anon | auto_unlink | create | debug | direct | fixed | lock | nocache | nolock | noreserve | populate | private | read | shared | shared_validate | sync | truncate | uninitialized | write | {address, pos_integer()} | {chmod, pos_integer()} | {size, pos_integer()}
</code></pre>

 Options for opening a memory mapped file:



<dt>anon</dt>




<dd>Anonymous mapping. The mapping is not backed by any file;
its contents are initialized to zero. The offset argument should be zero.</dd>




<dt>auto_unlink</dt>




<dd>Automatically delete the mapped file after the mapped data was garbage collected.
This can be used when the mapped file is a file-based shared-memory area (e.g. <code>/dev/shm/...</code>)
and is mapped in <code>direct</code> mode to free the memory after the data was gc'd</dd>




<dt>create</dt>




<dd>Allow to create mmap file if it doesn't exist.</dd>




<dt>debug</dt>




<dd>Turn on debug printing in the NIF library.</dd>




<dt>direct</dt>




<dd>Read/pread operations do not copy memory, but rather use "resource binaries" that
can change content if the underlying data is changed.  This is the most performant,
but also has other thread-safety implications when not using atomic operations.</dd>




<dt>fixed</dt>




<dd>Don't interpret addr as a hint: place the mapping at exactly that address.
The implementation aligns the given address to a multiple of the page size.</dd>




<dt>lock</dt>




<dd>Use a semaphore (read/write lock) to control state changes internally in the NIF
library. This is the default option.</dd>




<dt>nocache</dt>




<dd>Pages in this mapping are not retained in the kernel's memory cache.
If the system runs low on memory, pages in MAP_NOCACHE mappings will be among the
first to be reclaimed. NOTE: this option is only valid for Mac OS.</dd>




<dt>nolock</dt>




<dd>Don't use a semaphore (read/write lock) to control state changes internally in the NIF library</dd>




<dt>noreserve</dt>




<dd>Do not reserve swap space for this mapping.  When swap space is reserved, one has
the guarantee that it is possible to modify the mapping.</dd>




<dt>populate</dt>




<dd>Populate (prefault) page tables for a mapping.  For a file mapping, this causes
read-ahead on the file.  This will help to reduce blocking on page faults later.</dd>




<dt>private</dt>




<dd>Create a private copy-on-write mapping.  Updates to the mapping are not visible to
other processes mapping the same file, and are not carried through to the underlying
file.</dd>




<dt>read</dt>




<dd>Open for reading (this is default).</dd>




<dt>shared</dt>




<dd>Share this mapping.  Updates to the mapping are visible to other processes mapping
the same region, and (in the case of file-backed mappings) are carried through to
the underlying file. May be used in combination with <code>sync</code> to precisely control when
updates are carried through to the underlying file.</dd>




<dt>shared_validate</dt>




<dd>This flag provides the same behavior as <code>shared</code> except that <code>shared</code> mappings ignore
unknown flags in flags.  By contrast, when creating a mapping using <code>shared_validate</code>,
the kernel verifies all passed flags are known and fails the mapping with the error
<code>eopnotsupp</code> for unknown flags.  This mapping type is also required to be able to use
some mapping flags (e.g., <code>sync</code>)</dd>




<dt>sync</dt>




<dd>This flag is available only with the <code>shared_validate</code> mapping type; mappings of type
<code>shared</code> will silently ignore this flag.  This flag is supported only for files
supporting DAX (direct mapping of persistent memory).  For other files, creating a
mapping with this flag results in an <code>eopnotsupp</code> error.
Shared file mappings with this flag provide the guarantee that while some memory is
mapped writable in the address space of the process, it will be visible in the same
file at the same offset even after the system crashes or is rebooted.  In conjunction
with the use of appropriate CPU instructions, this provides users of such mappings
with a more efficient way of making data modifications persistent.</dd>




<dt>truncate</dt>




<dd>Truncate existing mmap file after it's open.</dd>




<dt>uninitialized</dt>




<dd>Don't clear anonymous pages.  This flag is intended to improve performance on
embedded devices.  This flag is honored only if the kernel was configured with
the <code>CONFIG_MMAP_ALLOW_UNINITIALIZED</code> option.</dd>




<dt>write</dt>




<dd>Open memory map for writing.</dd>




<dt>{address, pos_integer()}</dt>




<dd>Open mapping at the given memory address (sets <code>MAP_FIXED</code> on the memory mapped file)</dd>




<dt>{chmod,   pos_integer()}</dt>




<dd>Create mmap file with this mode (default: <code>0600</code>)</dd>




<dt>{size,    pos_integer()}</dt>




<dd>Create/access memory map on this size.</dd>





### <a name="type-resource">resource()</a> ###


<pre><code>
resource() = binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td></td></tr><tr><td valign="top"><a href="#close_counters-1">close_counters/1</a></td><td>Close persistent memory-mapped file previously open with <code>open_counters/2</code></td></tr><tr><td valign="top"><a href="#inc_counter-2">inc_counter/2</a></td><td>Increment a counter number <code>CounterNumber</code> in the mmap file by one and return old value.</td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td>Open/create a memory-mapped file.</td></tr><tr><td valign="top"><a href="#open-4">open/4</a></td><td>Open/create a memory-mapped file.</td></tr><tr><td valign="top"><a href="#open_counters-1">open_counters/1</a></td><td>Open a persistent memory-mapped file with space for one 64-bit integer counter.</td></tr><tr><td valign="top"><a href="#open_counters-2">open_counters/2</a></td><td>Open a persistent memory-mapped file with space for several 64-bit integer counters.</td></tr><tr><td valign="top"><a href="#patomic-4">patomic/4</a></td><td>Perform an atomic operation on a 64-bit integer value at given <code>Position</code>
using specified argument <code>Value</code>.</td></tr><tr><td valign="top"><a href="#patomic_read-2">patomic_read/2</a></td><td>Perform an atomic load operation on a 64-bit integer value at given <code>Position</code>.</td></tr><tr><td valign="top"><a href="#patomic_write-3">patomic_write/3</a></td><td>Perform an atomic store operation of a 64-bit integer <code>Value</code> at given <code>Position</code>.</td></tr><tr><td valign="top"><a href="#position-2">position/2</a></td><td>Write <code>Data</code> bytes to a memory-mapped file at a given offset <code>At</code>.</td></tr><tr><td valign="top"><a href="#pread-3">pread/3</a></td><td>Read <code>Len</code> bytes from a memory-mapped file at a given offset <code>Off</code>.</td></tr><tr><td valign="top"><a href="#pwrite-3">pwrite/3</a></td><td>Write <code>Data</code> bytes to a memory-mapped file at a given offset <code>Off</code>.</td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>Read next <code>Len</code> bytes from a memory-mapped file.</td></tr><tr><td valign="top"><a href="#read_line-1">read_line/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(File::<a href="#type-mmap_file">mmap_file()</a>) -&gt; ok
</code></pre>
<br />

<a name="close_counters-1"></a>

### close_counters/1 ###

`close_counters(X1) -> any()`

Close persistent memory-mapped file previously open with `open_counters/2`

<a name="inc_counter-2"></a>

### inc_counter/2 ###

`inc_counter(X1, CounterNumber) -> any()`

Increment a counter number `CounterNumber` in the mmap file by one and return old value.

<a name="init-0"></a>

### init/0 ###

`init() -> any()`

<a name="open-2"></a>

### open/2 ###

<pre><code>
open(FileName::string() | binary(), Options::[<a href="#type-open_option">open_option()</a>]) -&gt; {ok, <a href="#type-mmap_file">mmap_file()</a>} | {error, term()}
</code></pre>
<br />

Open/create a memory-mapped file.
If creating a new file, `[create, read, write, {size, N}]` options are required.
For opening an existing file for writing `[read, write]` options are required.

<a name="open-4"></a>

### open/4 ###

<pre><code>
open(File::string() | binary(), Offset::pos_integer(), Length::pos_integer(), Options::[<a href="#type-open_option">open_option()</a>]) -&gt; {ok, <a href="#type-mmap_file">mmap_file()</a>} | {error, term()}
</code></pre>
<br />

Open/create a memory-mapped file.
If creating a new file, `[create, read, write]` options and the `Len` parameter
are required.
For opening an existing file for writing `[read, write]` options are required, and `Len`
can be `0`.

<a name="open_counters-1"></a>

### open_counters/1 ###

`open_counters(Filename) -> any()`

Open a persistent memory-mapped file with space for one 64-bit integer counter

<a name="open_counters-2"></a>

### open_counters/2 ###

`open_counters(Filename, NumCounters) -> any()`

Open a persistent memory-mapped file with space for several 64-bit integer counters

<a name="patomic-4"></a>

### patomic/4 ###

<pre><code>
patomic(File::<a href="#type-mmap_file">mmap_file()</a>, Position::pos_integer(), Op::add | sub | band | bor | bxor | xchg, Value::integer()) -&gt; OldValue::integer()
</code></pre>
<br />

Perform an atomic operation on a 64-bit integer value at given `Position`
using specified argument `Value`.  The function returns an old value at that
location.  This function is thread-safe and can be used for implementing
persistent counters.

<a name="patomic_read-2"></a>

### patomic_read/2 ###

<pre><code>
patomic_read(File::<a href="#type-mmap_file">mmap_file()</a>, Position::pos_integer()) -&gt; Value::integer()
</code></pre>
<br />

Perform an atomic load operation on a 64-bit integer value at given `Position`.
This function is thread-safe and can be used for implementing persistent counters.

<a name="patomic_write-3"></a>

### patomic_write/3 ###

<pre><code>
patomic_write(File::<a href="#type-mmap_file">mmap_file()</a>, Position::pos_integer(), Value::integer()) -&gt; ok
</code></pre>
<br />

Perform an atomic store operation of a 64-bit integer `Value` at given `Position`.
This function is thread-safe and can be used for implementing persistent counters.

<a name="position-2"></a>

### position/2 ###

<pre><code>
position(File::<a href="#type-mmap_file">mmap_file()</a>, Position::pos_integer() | {bof | cur | eof, Position::integer()}) -&gt; {ok, pos_integer()} | {error, term()}
</code></pre>
<br />

Write `Data` bytes to a memory-mapped file at a given offset `At`.

<a name="pread-3"></a>

### pread/3 ###

<pre><code>
pread(File::<a href="#type-mmap_file">mmap_file()</a>, Offset::pos_integer(), Length::pos_integer()) -&gt; {ok, binary()} | {error, term()} | eof
</code></pre>
<br />

Read `Len` bytes from a memory-mapped file at a given offset `Off`.

<a name="pwrite-3"></a>

### pwrite/3 ###

<pre><code>
pwrite(File::<a href="#type-mmap_file">mmap_file()</a>, Position::pos_integer(), Data::binary()) -&gt; ok | {error, term()}
</code></pre>
<br />

Write `Data` bytes to a memory-mapped file at a given offset `Off`.

<a name="read-2"></a>

### read/2 ###

<pre><code>
read(File::<a href="#type-mmap_file">mmap_file()</a>, Length::pos_integer()) -&gt; {ok, binary()} | {error, term()} | eof
</code></pre>
<br />

Read next `Len` bytes from a memory-mapped file.
Internally the new position within the file is incremented by `Len`.

<a name="read_line-1"></a>

### read_line/1 ###

<pre><code>
read_line(File::<a href="#type-mmap_file">mmap_file()</a>) -&gt; {ok, binary()} | {error, term()} | eof
</code></pre>
<br />

