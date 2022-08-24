-module(emmap).

-export([
    init/0,
    open/2, open/4, close/1, resize/1, resize/2, flush/1,
    pread/3, pwrite/3, read/2, read_line/1, position/2,
    patomic_add/3,  patomic_sub/3, patomic_and/3, patomic_or/3, patomic_xor/3,
    patomic_xchg/3, patomic_cas/4,
    patomic_read_integer/2, patomic_write_integer/3
]).
-export([open_counters/1, open_counters/2, close_counters/1]).
-export([inc_counter/2, inc_counter/3, dec_counter/2, dec_counter/3]).
-export([set_counter/3, read_counter/2]).

-export_type([resource/0, mmap_file/0, open_option/0]).

-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("kernel/include/file.hrl").

-type open_option() ::
    anon
  | auto_unlink
  | create
  | debug
  | direct
  | fixed
  | lock
  | nocache
  | nolock
  | noreserve
  | populate
  | private
  | read
  | shared
  | shared_validate
  | sync
  | truncate
  | uninitialized
  | write
  | fixed_size
  | {address,      pos_integer()}
  | {chmod,        pos_integer()}
  | {size,         pos_integer()}
  | {max_inc_size, pos_integer()}.
%% Options for opening a memory mapped file:
%%
%% <dl>
%%   <dt>anon</dt>
%%    <dd>Anonymous mapping. The mapping is not backed by any file;
%%        its contents are initialized to zero. The offset argument should be zero.</dd>
%%   <dt>auto_unlink</dt>
%%    <dd>Automatically delete the mapped file after the mapped data was garbage collected.
%%        This can be used when the mapped file is a file-based shared-memory area (e.g. `/dev/shm/...')
%%        and is mapped in `direct' mode to free the memory after the data was gc'd</dd>
%%   <dt>create</dt>
%%    <dd>Allow to create mmap file if it doesn't exist.</dd>
%%   <dt>debug</dt>
%%    <dd>Turn on debug printing in the NIF library.</dd>
%%   <dt>direct</dt>
%%    <dd>Read/pread operations do not copy memory, but rather use "resource binaries" that
%%        can change content if the underlying data is changed.  This is the most performant,
%%        but also has other thread-safety implications when not using atomic operations.</dd>
%%   <dt>fixed</dt>
%%    <dd>Don't interpret addr as a hint: place the mapping at exactly that address.
%%        The implementation aligns the given address to a multiple of the page size.</dd>
%%   <dt>lock</dt>
%%    <dd>Use a semaphore (read/write lock) to control state changes internally in the NIF
%%        library. This is the default option.</dd>
%%   <dt>nocache</dt>
%%    <dd>Pages in this mapping are not retained in the kernel's memory cache.
%%        If the system runs low on memory, pages in MAP_NOCACHE mappings will be among the
%%        first to be reclaimed. NOTE: this option is only valid for Mac OS.</dd>
%%   <dt>nolock</dt>
%%    <dd>Don't use a semaphore (read/write lock) to control state changes internally in the NIF library</dd>
%%   <dt>noreserve</dt>
%%    <dd>Do not reserve swap space for this mapping.  When swap space is reserved, one has
%%        the guarantee that it is possible to modify the mapping.</dd>
%%   <dt>populate</dt>
%%    <dd>Populate (prefault) page tables for a mapping.  For a file mapping, this causes
%%        read-ahead on the file.  This will help to reduce blocking on page faults later.</dd>
%%   <dt>private</dt>
%%    <dd>Create a private copy-on-write mapping.  Updates to the mapping are not visible to
%%        other processes mapping the same file, and are not carried through to the underlying
%%        file.</dd>
%%   <dt>read</dt>
%%    <dd>Open for reading (this is default).</dd>
%%   <dt>shared</dt>
%%    <dd>Share this mapping.  Updates to the mapping are visible to other processes mapping
%%        the same region, and (in the case of file-backed mappings) are carried through to
%%        the underlying file. May be used in combination with `sync' to precisely control when
%%        updates are carried through to the underlying file.</dd>
%%   <dt>shared_validate</dt>
%%    <dd>This flag provides the same behavior as `shared' except that `shared' mappings ignore
%%        unknown flags in flags.  By contrast, when creating a mapping using `shared_validate',
%%        the kernel verifies all passed flags are known and fails the mapping with the error
%%        `eopnotsupp' for unknown flags.  This mapping type is also required to be able to use
%%        some mapping flags (e.g., `sync')</dd>
%%   <dt>sync</dt>
%%    <dd>This flag is available only with the `shared_validate' mapping type; mappings of type
%%        `shared' will silently ignore this flag.  This flag is supported only for files
%%        supporting DAX (direct mapping of persistent memory).  For other files, creating a
%%        mapping with this flag results in an `eopnotsupp' error.
%%        Shared file mappings with this flag provide the guarantee that while some memory is
%%        mapped writable in the address space of the process, it will be visible in the same
%%        file at the same offset even after the system crashes or is rebooted.  In conjunction
%%        with the use of appropriate CPU instructions, this provides users of such mappings
%%        with a more efficient way of making data modifications persistent.</dd>
%%   <dt>truncate</dt>
%%    <dd>Truncate existing mmap file after it's open.</dd>
%%   <dt>uninitialized</dt>
%%    <dd>Don't clear anonymous pages.  This flag is intended to improve performance on
%%        embedded devices.  This flag is honored only if the kernel was configured with
%%        the `CONFIG_MMAP_ALLOW_UNINITIALIZED' option.</dd>
%%   <dt>write</dt>
%%    <dd>Open memory map for writing.</dd>
%%   <dt>fixed_size</dt>
%%    <dd>Don't allow the memory to be resized</dd>
%%   <dt>{address, pos_integer()}</dt>
%%    <dd>Open mapping at the given memory address (sets `MAP_FIXED' on the memory mapped file)</dd>
%%   <dt>{chmod, pos_integer()}</dt>
%%    <dd>Create mmap file with this mode (default: `0600')</dd>
%%   <dt>{size, pos_integer()}</dt>
%%    <dd>Create/access memory map on this size.</dd>
%%   <dt>{max_inc_size, pos_integer()}</dt>
%%    <dd>Size threshold used when automatically resizing shared memory with call to `resize/1'.
%%        Below this threshold the memory will double, and after this threshold, the resized
%%        memory will be increased by `max_inc_size'.</dd>
%% </dl>

-type mmap_file() :: #file_descriptor{}.
-type resource()  :: binary().

-type open_extra_info() :: #{exist => boolean(), size => non_neg_integer()}.
%% Extra information returned by the call to `emmap:open/2,3'.
%% The value of `exist' true means that an existing memory map was open. The `size'
%% represents the size of the memory map that was open.

init() ->
    SoName =
        case code:priv_dir(emmap) of
            {error, bad_name} ->
                case code:which(?MODULE) of
                    Filename when is_list(Filename) ->
                        Dir = filename:dirname(filename:dirname(Filename)),
                        filename:join([Dir, "priv", "emmap"]);
                    _ ->
                        filename:join("../priv", "emmap")
                end;
            Dir ->
                filename:join(Dir, "emmap")
        end,
    erlang:load_nif(SoName, 0).

%% @doc Open/create a memory-mapped file.
%% If creating a new file, `[create, read, write, {size, N}]' options are required.
%% For opening an existing file for writing `[read, write]' options are required.
-spec open(string()|binary(), [open_option()]) ->
        {ok, mmap_file(), open_extra_info()} | {error, term()}.
open(FileName, Options) when is_binary(FileName) ->
    open(binary_to_list(FileName), Options);
open(FileName, Options) when is_list(FileName) ->
    case file:read_file_info(FileName) of
        {ok, FileInfo} ->
            open(FileName, 0, FileInfo#file_info.size, Options);
        _Error ->
            open(FileName, 0, 0, Options)
    end.

%% @doc Open/create a memory-mapped file.
%% If creating a new file, `[create, read, write]' options and the `Len' parameter
%% are required.
%% For opening an existing file for writing `[read, write]' options are required, and `Len'
%% can be `0'.
-spec open(File::string()|binary(),
          Offset::pos_integer(),
          Length::pos_integer(),
          Options::[ open_option() ]) ->
                 {ok, mmap_file(), open_extra_info()} | {error, term()}.
open(FileName, Off, Len, Options) when is_binary(FileName) ->
    open(binary_to_list(FileName), Off, Len, Options);
open(FileName, Off, Len, Options) when is_list(FileName), is_integer(Off), is_integer(Len) ->
    case open_nif(FileName, Off, Len, Options) of
        {ok, Mem, Info} ->
            {ok, #file_descriptor{module=?MODULE, data=Mem}, Info};
        {error, _} = Error ->
            Error
    end.

open_nif(_,_,_,_) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec close(File::mmap_file()) -> ok.
close(#file_descriptor{module=?MODULE, data=Mem}) -> close_nif(Mem).
close_nif(_) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Resize shared memory.
%% The new size will double the existing size up until the `max_inc_size' threshold (passed
%% to `open/4' (default 64M), otherwise incremented by `max_inc_size'.
-spec resize(File::mmap_file()) -> {ok, NewSize::non_neg_integer()} | {error, string()}.
resize(#file_descriptor{module=?MODULE, data=Mem}) -> resize_nif(Mem, 0).

%% @doc Resize shared memory to a given new size.
-spec resize(mmap_file(), non_neg_integer()) ->
        {ok, NewSize::non_neg_integer()} | {error, string()}.
resize(#file_descriptor{module=?MODULE, data=Mem}, NewSize) when is_integer(NewSize) ->
  resize_nif(Mem, NewSize).

resize_nif(_, _) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Ask the OS to flush the modified memory to disk. The call is asyncronous and
%% non-blocking.  This call is not required as the OS will asynchronously flush the
%% modified memory pages to disk lazily, but this call will trigger that process
%% immediately.
-spec flush(File::mmap_file()) -> ok.
flush(#file_descriptor{module=?MODULE, data=Mem}) -> sync_nif(Mem).
sync_nif(_) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Read `Len' bytes from a memory-mapped file at a given offset `Off'.
-spec pread(File::mmap_file(), Offset::pos_integer(), Length::pos_integer()) ->
                   {ok, binary()} | {error, term()} | eof.

pread(#file_descriptor{module=?MODULE, data=Mem}, Off, Len) -> pread_nif(Mem, Off, Len).
pread_nif(_,_,_) -> {ok, <<>>}.

%% @doc Read next `Len' bytes from a memory-mapped file.
%% Internally the new position within the file is incremented by `Len'.
-spec read(File::mmap_file(), Length::pos_integer()) ->
                   {ok, binary()} | {error, term()} | eof.

read(#file_descriptor{module=?MODULE, data=Mem}, Len) -> read_nif(Mem, Len).
read_nif(_,_) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).


-spec read_line(File::mmap_file()) ->
                   {ok, binary()} | {error, term()} | eof.

read_line(#file_descriptor{module=?MODULE, data=Mem}) -> read_line_nif(Mem).
read_line_nif(_) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Write `Data' bytes to a memory-mapped file at a given offset `Off'.
-spec pwrite(File::mmap_file(), Position::pos_integer(), Data::binary()) ->
                    ok | {error, term()}.
pwrite(#file_descriptor{module=?MODULE, data=Mem}, Off, Data) -> pwrite_nif(Mem, Off, Data).
pwrite_nif(_,_,_) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Write `Data' bytes to a memory-mapped file at a given offset `At'.
-spec position(File::mmap_file(),
               Position::pos_integer() | {bof|cur|eof, Position::integer()} ) ->
                    {ok, pos_integer()} | {error, term()}.
position(#file_descriptor{module=?MODULE, data=Mem}, At)
  when is_integer(At) ->
    position_nif(Mem, bof, At);
position(#file_descriptor{module=?MODULE, data=Mem}, From)
  when From == 'bof'; From == 'cur'; From == 'eof' ->
    position_nif(Mem, From, 0);
position(#file_descriptor{module=?MODULE, data=Mem}, {From, Off})
  when From == 'bof'; From == 'cur'; From == 'eof' ->
    position_nif(Mem, From, Off).

position_nif(_,_From,_Off) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Perform an atomic ADD operation on a 64-bit integer value at given `Position'
%% using specified argument `Value'.  The function returns an old value at that
%% location.  This function is thread-safe and can be used for implementing
%% persistent counters.
-spec patomic_add(File::mmap_file(), Position::pos_integer(), Value::integer()) ->
        {ok, OldValue::integer()} | {error, atom()} | no_return().
patomic_add(#file_descriptor{module=?MODULE, data=Mem}, Off, Value)
  when is_integer(Off), is_integer(Value) ->
    patomic_add_nif(Mem, Off, Value).

patomic_add_nif(_,_,_) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Perform an atomic SUBTRACT operation on a 64-bit integer value at given `Position'
%% using specified argument `Value'.  The function returns an old value at that
%% location.  This function is thread-safe and can be used for implementing
%% persistent counters.
-spec patomic_sub(File::mmap_file(), Position::pos_integer(), Value::integer()) ->
        {ok, OldValue::integer()} | {error, atom()} | no_return().
patomic_sub(#file_descriptor{module=?MODULE, data=Mem}, Off, Value)
  when is_integer(Off), is_integer(Value) ->
    patomic_sub_nif(Mem, Off, Value).

patomic_sub_nif(_,_,_) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Perform an atomic AND operation on a 64-bit integer value at given `Position'
%% using specified argument `Value'.  The function returns an AND'd value at that
%% location.
-spec patomic_and(File::mmap_file(), Position::pos_integer(), Value::integer()) ->
        {ok, integer()} | {error, atom()} | no_return().
patomic_and(#file_descriptor{module=?MODULE, data=Mem}, Off, Value)
  when is_integer(Off), is_integer(Value) ->
    patomic_and_nif(Mem, Off, Value).

patomic_and_nif(_,_,_) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Perform an atomic OR operation on a 64-bit integer value at given `Position'
%% using specified argument `Value'.  The function returns an OR'd value at that
%% location.
-spec patomic_or(File::mmap_file(), Position::pos_integer(), Value::integer()) ->
        {ok, integer()} | {error, atom()} | no_return().
patomic_or(#file_descriptor{module=?MODULE, data=Mem}, Off, Value)
  when is_integer(Off), is_integer(Value) ->
    patomic_or_nif(Mem, Off, Value).

patomic_or_nif(_,_,_) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Perform an atomic XOR operation on a 64-bit integer value at given `Position'
%% using specified argument `Value'.  The function returns an XOR'd value at that
%% location.
-spec patomic_xor(File::mmap_file(), Position::pos_integer(), Value::integer()) ->
        {ok, integer()} | {error, atom()} | no_return().
patomic_xor(#file_descriptor{module=?MODULE, data=Mem}, Off, Value)
  when is_integer(Off), is_integer(Value) ->
    patomic_xor_nif(Mem, Off, Value).

patomic_xor_nif(_,_,_) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Perform an atomic EXCHANGE operation on a 64-bit integer value at given `Position'
%% using specified argument `Value'.  The function returns an old value at that
%% location.
-spec patomic_xchg(File::mmap_file(), Position::pos_integer(), Value::integer()) ->
        {ok, integer()} | {error, atom()} | no_return().
patomic_xchg(#file_descriptor{module=?MODULE, data=Mem}, Off, Value)
  when is_integer(Off), is_integer(Value) ->
    patomic_xchg_nif(Mem, Off, Value).

patomic_xchg_nif(_,_,_) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Perform an atomic compare and swap (CAS) operation on a 64-bit integer value
%% at given `Position' using specified argument `Value'.  The function returns a
%% tuple `{Success, OldVal}', where `OldVal' is the old value at that location.
-spec patomic_cas(File::mmap_file(), Position::pos_integer(), integer(), integer()) ->
        {boolean(), integer()} | {error, atom()} | no_return().
patomic_cas(#file_descriptor{module=?MODULE, data=Mem}, Off, OldValue, Value)
  when is_integer(Off), is_integer(OldValue), is_integer(Value) ->
    patomic_cas_nif(Mem, Off, OldValue, Value).

patomic_cas_nif(_,_,_,_) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Perform an atomic store operation of a 64-bit integer `Value' at given `Position'.
%% This function is thread-safe and can be used for implementing persistent counters.
-spec patomic_write_integer(File::mmap_file(), Position::pos_integer(), Value::integer()) -> ok.
patomic_write_integer(#file_descriptor{module=?MODULE, data=Mem}, Off, Value)
  when is_integer(Off), is_integer(Value) ->
    patomic_write_int_nif(Mem, Off, Value).

patomic_write_int_nif(_,_,_) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Perform an atomic load operation on a 64-bit integer value at given `Position'.
%% This function is thread-safe and can be used for implementing persistent counters.
-spec patomic_read_integer(File::mmap_file(), Position::pos_integer()) -> Value::integer().
patomic_read_integer(#file_descriptor{module=?MODULE, data=Mem}, Off) when is_integer(Off) ->
    patomic_read_int_nif(Mem, Off).

patomic_read_int_nif(_,_) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Open a persistent memory-mapped file with space for one 64-bit integer counter
open_counters(Filename) ->
    open_counters(Filename, 1).

%% @doc Open a persistent memory-mapped file with space for several 64-bit integer counters
open_counters(Filename, NumCounters) ->
    Size     = 8 + 8 + NumCounters * 8,
    FileSize = filelib:file_size(Filename),
    MMAP     = 
        case open(Filename, 0, Size, [create, read, write, shared, direct, nolock]) of
            {ok, F, #{exist := false}} when NumCounters > 0 ->
                ok = pwrite(F, 0, <<"EMMAP01\n", NumCounters:64/little-integer>>),
                {F, NumCounters};
            {ok, F, #{exist := true}} ->
                case pread(F, 0, 16) of
                    {ok, <<"EMMAP", _,_,$\n, N:64/little-integer>>} when N == NumCounters; NumCounters == 0 ->
                        {F, N};
                    {ok, _Other} when FileSize == Size ->
                        io:format("Initializing mmap: ~p\n", [_Other]),
                        ok = pwrite(F, 0, <<"EMMAP01\n", NumCounters:64/little-integer>>),
                        lists:foldl(fun(I, _) ->
                            ok = pwrite(F, 16+I*8, <<0:64/little-integer>>),
                            []
                        end, [], lists:seq(0, NumCounters-1)),
                        {F, NumCounters};
                    {error, Why} ->
                        erlang:error({cannot_read_data, Filename, Why})
                end;
            {error, Reason} ->
                erlang:error({cannot_open_file, Filename, Reason})
        end,
    MMAP.

%% @doc Close persistent memory-mapped file previously open with `open_counters/2'
close_counters({MFile, _NumCnts}) ->
    close(MFile).

%% @doc Increment a counter number `CounterNum' in the mmap file by one and return old value.
inc_counter({MFile, NumCnts}, CounterNum) ->
    inc_counter({MFile, NumCnts}, CounterNum, 1).

%% @doc Decrement a counter number `CounterNum' in the mmap file by one and return old value.
dec_counter({MFile, NumCnts}, CounterNum) ->
    dec_counter({MFile, NumCnts}, CounterNum, 1).

%% @doc Increment a counter number `CounterNum' in the mmap file by `Count' and return old value.
inc_counter({MFile, NumCnts}, CounterNum, Count)
        when NumCnts > CounterNum, CounterNum >= 0, is_integer(CounterNum), is_integer(Count) ->
    patomic_add(MFile, 16+CounterNum*8, Count).

%% @doc Decrement a counter number `CounterNum' in the mmap file by `Count' and return old value.
dec_counter({MFile, NumCnts}, CounterNum, Count)
        when NumCnts > CounterNum, CounterNum >= 0, is_integer(CounterNum), is_integer(Count) ->
    patomic_sub(MFile, 16+CounterNum*8, Count).

%% @doc Set a counter number `CounterNum' in the mmap file and return the old value.
set_counter({MFile, NumCnts}, CounterNum, Value)
        when NumCnts > CounterNum, CounterNum >= 0, is_integer(CounterNum), is_integer(Value) ->
    patomic_xchg(MFile, 16+CounterNum*8, Value).

read_counter({MFile, NumCnts}, CounterNum)
        when NumCnts > CounterNum, CounterNum >= 0, is_integer(CounterNum) ->
    patomic_read_integer(MFile, 16+CounterNum*8).
  
-ifdef(EUNIT).

simple_test() ->
    {ok, File} = file:open("test.data", [raw, write]),
    ok = file:write(File, <<"abcd0123">>),
    ok = file:close(File),

    %% with direct+shared, the contents of a binary may change
    {ok, MFile, #{size := 8}} = emmap:open("test.data", 0, 8, [direct, shared, nolock]),
    {ok, Mem}       = file:pread(MFile, 2, 2),
    <<"cd">>        = Mem,
    {error, eacces} = file:pwrite(MFile, 2, <<"xx">>),

    {ok, MFile2, #{size := 8}} = emmap:open("test.data", 0, 8, [read, write, shared]),
    ok                 = file:pwrite(MFile2, 2, <<"xx">>),
    {ok,     <<"xx">>} = file:pread(MFile, 2, 2),

    %% Woot!
    <<"xx">> = Mem,

    {ok, 0} = file:position(MFile, {cur, 0}),
    {ok, <<"ab">>} = file:read(MFile, 2),
    {ok, <<"xx">>} = file:read(MFile, 2),

    ok = file:pwrite(MFile2, 0, <<0:64>>),
    {ok, <<0:64>>} = file:pread(MFile, 0, 8),

    {ok,  0}    = emmap:patomic_add(MFile2,  0,10),
    {ok, 10}    = emmap:patomic_add(MFile2,  0,10),
    {ok, 20}    = emmap:patomic_sub(MFile2,  0, 5),
    {ok, 15}    = emmap:patomic_sub(MFile2,  0,12),
    {ok,  3}    = emmap:patomic_and(MFile2,  0, 7),
    {ok,  3}    = emmap:patomic_or(MFile2,   0, 7),
    {ok,  7}    = emmap:patomic_xor(MFile2,  0, 9),
    {ok, 14}    = emmap:patomic_xchg(MFile2, 0,10),
    {ok, 10}    = emmap:patomic_xchg(MFile2, 0, 0),
    {true,   0} = emmap:patomic_cas(MFile2,  0, 0,  20),
    {false, 20} = emmap:patomic_cas(MFile2,  0, 10, 20),
    {true,  20} = emmap:patomic_cas(MFile2,  0, 20,  0),

    file:close(MFile),
    file:close(MFile2),
    
    {ok, MFile3, #{exist := true, size := 8}} = emmap:open("test.data", 0, 8,
        [direct, read, write, shared, nolock, {address, 16#512800000000}]),
    {ok, <<0:64>>} = file:pread(MFile3, 0, 8),
    file:close(MFile3),

    file:delete("test.data").

counter_test() ->
    F  = open_counters("/tmp/temp.bin", 1),
    {ok,N1} = inc_counter(F, 0, 1),
    {ok,N2} = inc_counter(F, 0, 1),
    {ok,N3} = set_counter(F, 0, 5),
    {ok,N4} = set_counter(F, 0, 8),
    {ok,N5} = read_counter(F, 0),
    {ok,N6} = dec_counter(F, 0),
    {ok,N7} = dec_counter(F, 0, 3),
    {ok,N8} = read_counter(F, 0),
    close_counters(F),
    file:delete("/tmp/temp.bin"),
    ?assertEqual(0, N1),
    ?assertEqual(1, N2),
    ?assertEqual(2, N3),
    ?assertEqual(5, N4),
    ?assertEqual(8, N5),
    ?assertEqual(8, N6),
    ?assertEqual(7, N7),
    ?assertEqual(4, N8).

shared_test() ->
    F = fun(Owner) ->
          {ok, MM, #{size := 8}} = emmap:open("test.data", 0, 8, [create, direct, read, write, shared, nolock]),
          Two = receive {start, PP} -> PP end,
          ok = emmap:pwrite(MM, 0, <<"test1">>),
          Two ! {self(), <<"test1">>},
          receive {cont, Two} -> ok end,
          ok = emmap:pwrite(MM, 0, <<"test2">>),
          Two ! {self(), <<"test2">>},
          receive {cont, Two} -> ok end,
          Two   ! {done, 1},
          Owner ! {done, MM}
        end,
    G = fun(One, Owner) ->
          {ok, MM, #{size := 8}} = emmap:open("test.data", 0, 8, [create, direct, read, write, shared, nolock]),
          One ! {start, self()},
          Bin =
            receive
              {One, Bin1 = <<"test1">>} ->
                {ok, B} = emmap:pread(MM, 0, byte_size(Bin1)),
                B;
              Other1 ->
                erlang:error({error, {one, Other1}})
            end,
          % At this point value of Bin is this:
          Bin = <<"test1">>,
          One ! {cont, self()},
          receive
            {One, Bin2 = <<"test2">>} when Bin2 == Bin ->
              % Note that previously bound binary changed under the hood
              % because it's bound to the memory updated by another process
              Bin = <<"test2">>;
            Other2 ->
              erlang:error({error, {two, Other2}})
          end,
          One ! {cont, self()},
          receive
            {done, 1} -> ok
          end,
          Owner ! {done, MM}
        end,
    Self = self(),
    P1 = spawn_link(fun() -> F(Self)     end),
    _  = spawn_link(fun() -> G(P1, Self) end),

    receive {done, MM1} -> emmap:close(MM1) end,
    receive {done, MM2} -> emmap:close(MM2) end,

    file:delete("test.data").
    

-endif.
