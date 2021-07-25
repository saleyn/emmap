

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
open_option() = read | write | create | truncate | {chmod, pos_integer()} | {size, pos_integer()} | direct | lock | nolock | private | shared | populate | anon | fixed | nocache | noreserve | nocache | auto_unlink | {address, pos_integer()}
</code></pre>




### <a name="type-resource">resource()</a> ###


<pre><code>
resource() = binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td></td></tr><tr><td valign="top"><a href="#close_counters-1">close_counters/1</a></td><td></td></tr><tr><td valign="top"><a href="#inc_counter-2">inc_counter/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td></td></tr><tr><td valign="top"><a href="#open-4">open/4</a></td><td></td></tr><tr><td valign="top"><a href="#open_counters-1">open_counters/1</a></td><td>Open a persistent memory-mapped file with space for one 64-bit integer counter.</td></tr><tr><td valign="top"><a href="#open_counters-2">open_counters/2</a></td><td>Open a persistent memory-mapped file with space for several 64-bit integer counters.</td></tr><tr><td valign="top"><a href="#patomic-4">patomic/4</a></td><td>Perform an atomic operation on a 64-bit integer value at given <code>Position</code>
using specified argument <code>Value</code>.</td></tr><tr><td valign="top"><a href="#position-2">position/2</a></td><td></td></tr><tr><td valign="top"><a href="#pread-3">pread/3</a></td><td></td></tr><tr><td valign="top"><a href="#pwrite-3">pwrite/3</a></td><td></td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td></td></tr><tr><td valign="top"><a href="#read_line-1">read_line/1</a></td><td></td></tr></table>


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

<a name="inc_counter-2"></a>

### inc_counter/2 ###

`inc_counter(X1, CounterNumber) -> any()`

<a name="init-0"></a>

### init/0 ###

`init() -> any()`

<a name="open-2"></a>

### open/2 ###

`open(FileName, Options) -> any()`

<a name="open-4"></a>

### open/4 ###

<pre><code>
open(File::string(), Offset::pos_integer(), Length::pos_integer(), Options::[<a href="#type-open_option">open_option()</a>]) -&gt; {ok, <a href="#type-mmap_file">mmap_file()</a>} | {error, term()}
</code></pre>
<br />

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

<a name="position-2"></a>

### position/2 ###

<pre><code>
position(File::<a href="#type-mmap_file">mmap_file()</a>, Position::pos_integer() | {bof | cur | eof, Position::integer()}) -&gt; {ok, pos_integer()} | {error, term()}
</code></pre>
<br />

<a name="pread-3"></a>

### pread/3 ###

<pre><code>
pread(File::<a href="#type-mmap_file">mmap_file()</a>, Offset::pos_integer(), Length::pos_integer()) -&gt; {ok, binary()} | {error, term()} | eof
</code></pre>
<br />

<a name="pwrite-3"></a>

### pwrite/3 ###

<pre><code>
pwrite(File::<a href="#type-mmap_file">mmap_file()</a>, Position::pos_integer(), Data::binary()) -&gt; ok | {error, term()}
</code></pre>
<br />

<a name="read-2"></a>

### read/2 ###

<pre><code>
read(File::<a href="#type-mmap_file">mmap_file()</a>, Length::pos_integer()) -&gt; {ok, binary()} | {error, term()} | eof
</code></pre>
<br />

<a name="read_line-1"></a>

### read_line/1 ###

<pre><code>
read_line(File::<a href="#type-mmap_file">mmap_file()</a>) -&gt; {ok, binary()} | {error, term()} | eof
</code></pre>
<br />

