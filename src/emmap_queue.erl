%%%-----------------------------------------------------------------------------
%%% @doc     Persistent FIFO queue
%%%          The FIFO queue can be used as a persistent container of messages
%%%          with constant time push and pop operations.  Additionally, this
%%%          module provides a gen_server API, which wraps the queue for use
%%%          in multi-process applications.
%%%          The queue is stored in a memory-mapped file, and it automatically
%%%          grows if the messages are not dequeued from the queue.  Messages
%%%          stored in the queue can be compressed using variable compression
%%%          level controlled by the argument to the `push/3' function.
%%%          See test cases at the end of this module for sample use cases.
%%% @author  Serge Aleynikov
%%% @end
%%%-----------------------------------------------------------------------------
%%% Created: 2021-12-10
%%%-----------------------------------------------------------------------------
-module(emmap_queue).
-export([open/3, close/1, flush/1]).
-export([purge/1, is_empty/1, length/1, push/2, push/3,
         pop/1, pop/3, peek_front/1, peek_back/1, peek/3, rpeek/3, try_pop/2,
         pop_and_purge/1, try_pop_and_purge/2]).

-export([start_link/4, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([enqueue/2, dequeue/1, try_dequeue/2, inspect/1, info/1]).

-compile({no_auto_import,[length/1]}).

-behavior(gen_server).

-define(HEADER_SZ, 16).
-define(INIT_SEGM_HEADER,
        <<?HEADER_SZ:32/integer, ?HEADER_SZ:32/integer, ?HEADER_SZ:32/integer>>).
-define(INIT_SEGM_HEADER_WITH_SIZE(Size),
        <<Size:32/integer, ?HEADER_SZ:32/integer, ?HEADER_SZ:32/integer, ?HEADER_SZ:32/integer>>).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("kernel/include/file.hrl").

-record(state, {
    filename :: string()
  , mem      :: emmap:mmap_file()
  , segm_sz  :: non_neg_integer()
}).

-define(TIMEOUT, infinity).

%%------------------------------------------------------------------------------
%% Gen_server API
%%------------------------------------------------------------------------------

%% @doc Start a queue process.
%% `Name' - name of the registered process.
%% `Filename' - name of the memory mapped file.
%% `SegmSize' - size of the memory mapped segment (can be 0 for an existing file).
%% `Opts' - see `emmap:open_options()'.
start_link(Name, Filename, SegmSize, Opts) when is_list(Opts) ->
  gen_server:start_link({local, Name}, ?MODULE, [Filename, SegmSize, Opts], []).

%% @doc Enqueue a message to the queue
enqueue(Name, Term) ->
  gen_server:call(Name, {push, Term}, ?TIMEOUT).

%% @doc Dequeue a message from the queue
dequeue(Name) ->
  gen_server:call(Name, pop, ?TIMEOUT).

%% @doc Dequeue a message from the queue.
%% The function returns `Res' where `Res' is the result of evaluating
%% the `Fun' with the poped element from the queue.  If the `Fun' throws an
%% exception, the exception if propagated to the caller.
try_dequeue(Name, Fun) when is_function(Fun, 1) ->
  case gen_server:call(Name, {try_pop, Fun}, ?TIMEOUT) of
    {ok, Res} ->
      Res;
    {error, Error, Stacktrace} ->
      erlang:error(Error, Stacktrace)
  end.

%% @doc Peek a message from the queue without dequeuing it
inspect(Name) ->
  gen_server:call(Name, peek, ?TIMEOUT).

%% @doc Return queue info as a map
-spec info(atom()) -> map().
info(Name) ->
  gen_server:call(Name, info, ?TIMEOUT).

%%------------------------------------------------------------------------------
%% Gen_server callbacks
%%------------------------------------------------------------------------------

init([Filename, SegmSize, Opts]) ->
  erlang:process_flag(trap_exit, true),
  {ok, Mem}  = open(Filename, SegmSize, Opts),
  {ok, #state{mem=Mem, filename=Filename, segm_sz=SegmSize}}.

handle_call({push, Term}, _From, #state{mem=Mem} = State) ->
  case push(Mem, Term) of
    ok ->
      {reply, ok, State};
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;

handle_call(pop, _From, #state{mem=Mem} = State) ->
  Msg = pop_and_purge(Mem),
  {reply, Msg, State};

handle_call(peek, _From, #state{mem=Mem} = State) ->
  Msg = peek_front(Mem),
  {reply, Msg, State};

handle_call({try_pop, Fun}, _From, #state{mem=Mem} = State) ->
  try
    {reply, {ok, try_pop_and_purge(Mem, Fun)}, State}
  catch _:E:ST ->
    {reply, {error, E, ST}, State}
  end;

handle_call(info, _From, #state{mem=Mem} = State) ->
  {Head, Tail, NextTail, SegmSize} = header(Mem),
  {reply, #{head => Head, tail => Tail, next_tail => NextTail, size => SegmSize}, State}.

handle_cast(Msg, State) ->
  {stop, {cast_not_implemented, Msg}, State}.

handle_info({'EXIT', _Pid, Reason}, State) ->
  {stop, Reason, State}.

terminate(_Reason, #state{mem=Mem}) ->
  close(Mem).

%%------------------------------------------------------------------------------
%% Raw segment access functions
%%------------------------------------------------------------------------------

%% @doc Open a memory mapped queue.
open(Filename, Size, Opts) when is_list(Filename), is_integer(Size), is_list(Opts) ->
  ok = filelib:ensure_dir(filename:dirname(Filename)),
  case emmap:open(Filename, 0, Size, [create, read, write | Opts]) of
    {ok, Mem, #{exist := true, size := SegmSize}} ->
      case header(Mem) of
        {_Head, Tail, Tail, SSize} when SSize =/= SegmSize ->
          %% Repair size
          update_size(Mem, SegmSize),
          {ok, Mem};
        {_Head, Tail, Tail, _SegmSize} ->
          {ok, Mem};
        {_Head, Tail, _NextTail, _SegmSize} ->
          % Need to repair last uncommitted tail
          reserve_tail(Mem, Tail),
          {ok, Mem}
      end;
    {ok, Mem, #{exist := false, size := SegmSize}} ->
      ok = emmap:pwrite(Mem, 0, ?INIT_SEGM_HEADER_WITH_SIZE(SegmSize)),
      {ok, Mem};
    Error ->
      Error
  end.

%% @doc Close a previously open queue.
close(Mem) ->
  emmap:close(Mem).

%% @doc Asynchronously flush the modified memory used by the queue to disk.
%% See notes of `emmap:sync/1'.  This call is optional.
flush(Mem) ->
  emmap:flush(Mem).

%% @doc Purge queue.  It is a constant time operation.
purge(Mem) ->
  case is_empty_queue(Mem) of
    {true, 0} ->
      true;
    {true, _} ->
      init_header(Mem),
      true;
    false ->
      false
  end.

header(Mem) ->
  case emmap:pread(Mem, 0, 16) of
    {ok, <<Size:32/integer, H:32/integer, T:32/integer, NextT:32/integer>>}
        when H >= ?HEADER_SZ, T >= ?HEADER_SZ, H =< T, T =< Size, NextT >= T, NextT =< Size ->
      {H, T, NextT, Size};
    {ok, Hdr} ->
      erlang:error({invalid_queue_header, Hdr});
    {error, Err} ->
      erlang:error({cannot_read_header, Err})
  end.

%% @doc Returns `true' if the queue is empty.
is_empty(Mem) ->
  case is_empty_queue(Mem) of
    {true, _Head} -> true;
    false         -> false
  end.

is_empty_queue(#file_descriptor{} = Mem) ->
  case emmap:pread(Mem, 0, 16) of
    {ok, Header} ->
      is_empty_queue(Header);
    {error, Error} ->
      erlang:error(Error)
  end;
is_empty_queue(<<_Sz:4/binary, H:32/integer, H:32/integer, _/binary>>) ->
  {true, H};
is_empty_queue(_Header) ->
  false.

%% @doc Get queue length. This function has a linear-time complexity.
length(Mem) ->
  F = fun
    G(H, T, Count) when H < T ->
      {ok, <<Sz:32/integer>>} = emmap:pread(Mem, H, 4),
      G(H + Sz, T, Count+1);
    G(_, _, Count) ->
      Count
  end,
  {Head, Tail, _, _} = header(Mem),
  F(Head, Tail, 0).

%% @doc Push a term to the queue. This function has a constant-time complexity.
push(Mem, Term) ->
  push(Mem, Term, 0).

%% @doc Push a term to the queue. This function has a constant-time complexity.
%% `Compression' is the compression level from `0' to `9', where `0' is no compression.
push(Mem, Term, Compression) when is_integer(Compression), Compression >= 0, Compression < 10 ->
  Bin = term_to_binary(Term, [{compressed, 0}, {minor_version, 2}]),
  Sz0 = byte_size(Bin)+8,
  Pad = Sz0 rem 8,
  Sz  = Sz0 + Pad,
  Msg = <<Sz:32/integer, Bin/binary, 0:(Pad*8)/integer, Sz:32/integer>>,
  Arg = header(Mem),
  (fun
    G({_Head,_Tail, NextTail, SegmSize}) when NextTail+Sz > SegmSize ->
      case emmap:resize(Mem) of
        {ok, NewSegmSize} ->
          update_size(Mem, NewSegmSize),
          G(header(Mem));
        {error, fixed_size} ->
          {error, full};    % Queue is full and was given fixed_size option when opened
        {error, Reason} ->
          {error, Reason}
      end;
    G({_Head, Tail, NextTail,_SegmSize}) ->
      NewTail = NextTail+Sz,
      TailBin = <<NewTail:32/integer>>,
      reserve_tail(Mem, TailBin),            % Reserve the next tail offset
      ok = emmap:pwrite(Mem, Tail, Msg),     % Write message
      commit_tail(Mem, TailBin)              % Commit tail offset
  end)(Arg).

%% @doc Pop a term from the queue. This function has a constant-time complexity.
pop(Mem) ->
  case read_head(Mem, true) of
    {Msg, _IsEnd} ->
      Msg;
    nil ->
      nil
  end.

%% @doc Evaluate the `Fun' function on the next term in the queue.
%% If the function doesn't raise an exception, pop the term from the queue, otherwise
%% leave the queue unmodified. This function has a constant-time complexity. It returns
%% the result of evaluating `Fun'.
try_pop(Mem, Fun) when is_function(Fun, 1) ->
  case read_head(Mem, Fun) of
    {Res, _IsEnd} ->
      Res;
    nil ->
      nil
  end.

%% @doc Evaluate the `Fun' function on the next term in the queue.
%% If the function doesn't raise an exception, pop the term from the queue, otherwise
%% leave the queue unmodified. This function has a constant-time complexity. It returns
%% the result of evaluating `Fun'.
try_pop_and_purge(Mem, Fun) when is_function(Fun, 1) ->
  case read_head(Mem, Fun) of
    {Res, true} ->
      init_header(Mem),
      Res;
    {Res, false} ->
      Res;
    nil ->
      nil
  end.

%% @doc Peek the next awaiting term from the head of the FIFO queue without popping it.
%% This function has a constant-time complexity.
peek_front(Mem) ->
  case read_head(Mem, false) of
    {Msg, _IsEnd} ->
      Msg;
    nil ->
      nil
  end.

%% @doc Peek the last written term at the back of the FIFO queue without removing it.
%% This function has a constant-time complexity.
peek_back(Mem) ->
  case header(Mem) of
    {Head, Tail, _NextTail, _Size} ->
      {Msg, _PrevT} = read_last(Mem, Head, Tail),
      Msg;
    _ ->
      nil
  end.
 
%% @doc Pop a term from the queue and reclaim queue's memory if the queue is empty.
%% This function has a constant-time complexity.
pop_and_purge(Mem) ->
  case read_head(Mem, true) of
    {Msg, true} ->
      init_header(Mem),
      Msg;
    {Msg, false} ->
      Msg;
    nil ->
      nil
  end.

%% @doc Inspect all messages in the queue iteratively by passing them to a custom
%% lambda function. The `Fun' takes a message and state and returns a `{cont, State}'
%% to continue or `{halt, State}' to abort.
peek(Mem, Init, Fun) when is_function(Fun, 2) ->
  F = fun
    G(H, T, State) when H < T ->
      {Msg, NextH} = read_first(Mem, H),
      case Fun(Msg, State) of
        {cont, State1} ->
          G(NextH, T, State1);
        {halt, State1} ->
          State1
      end;
    G(_, _, State) ->
      State
  end,
  {Head, Tail, _, _} = header(Mem),
  F(Head, Tail, Init).

%% @doc Inspect all messages in the queue iteratively in the reverse order by passing 
%% them to a custom lambda function. The `Fun' takes a message and state and returns a
%% `{cont, State}' to continue or `{halt, State}' to abort.
rpeek(Mem, Init, Fun) when is_function(Fun, 2) ->
  F = fun G(H, T, State) ->
    case read_last(Mem, H, T) of
      {nil, _} ->
        State;
      {Msg, PrevT} -> 
        case Fun(Msg, State) of
          {cont, State1} ->
            G(H, PrevT, State1);
          {halt, State1} ->
            State1
        end
    end
  end,
  {Head, Tail, _, _} = header(Mem),
  F(Head, Tail, Init).

%% @doc Pop messages from the queue by passing them to a custom lambda function.
%% The `Fun' takes a message and state and returns a `{cont, State}' to continue or
%% `{halt, State}' to abort.
pop(Mem, Init, Fun) when is_function(Fun, 2) ->
  F = fun
    G(H, T, State) when H < T ->
      {Msg, NextH} = read_first(Mem, H),
      case Fun(Msg, State) of
        {cont, State1} ->
          update_head(Mem, NextH),
          G(NextH, T, State1);
        {halt, State1} ->
          State1
      end;
    G(?HEADER_SZ, _, State) ->
      State;
    G(_, _, State) ->
      % Purge the empty queue
      init_header(Mem),
      State
  end,
  {Head, Tail, _, _} = header(Mem),
  F(Head, Tail, Init).

read_head(Mem, Pop) ->
  case header(Mem) of
    {Head,  Tail, NextTail, _SegmSize} when Head < Tail ->
      {Msg, NextHead} = read_first(Mem, Head),
      IsEnd  = (NextHead =:= Tail) andalso (NextHead =:= NextTail),
      case Pop of
        true ->
          update_head(Mem, NextHead),
          {Msg, IsEnd};
        false ->
          {Msg, IsEnd};
        _ when is_function(Pop, 1) ->
          Res = Pop(Msg),
          update_head(Mem, NextHead),
          {Res, IsEnd}
      end;
    {_Head, _Tail, _NextTail, _} ->
      nil
  end.

read_first(Mem, Head) ->
  % Read the size of the next message
  {ok,   <<Sz:32/integer>>} = emmap:pread(Mem, Head, 4),
  BinSz  = Sz-8,  % The size read is inclusive of the 2 sizes written before/after the msg
  {ok,   <<Bin:BinSz/binary, EndSz:32/integer>>} = emmap:pread(Mem, Head+4, BinSz+4),
  EndSz /= Sz   andalso erlang:error({message_size_mismatch, {Sz, EndSz}, Head}),
  {binary_to_term(Bin), Head+Sz}.
 
read_last(Mem, Head, Tail) when Head < Tail-8 ->
  % Read the size of the next message
  {ok, <<Sz:32/integer>>} = emmap:pread(Mem, Tail-4, 4),
  case Tail-Sz of
    I when I >= Head ->
      BinSz  = Sz-8,  % The size read is inclusive of the 2 sizes written before/after the msg
      case emmap:pread(Mem, I, Sz-4) of
        {ok, <<Sz:32/integer, Bin:BinSz/binary>>} ->
          {binary_to_term(Bin), I};
        {error, Why} ->
          erlang:error({invalid_msg_header, Why})
      end;
    I ->
      {nil, I}
  end;
read_last(_Mem, I, I) ->
  {nil, I}.

update_head(Mem, NextHead) ->
  ok = emmap:pwrite(Mem, 4, <<NextHead:32/integer>>).

update_size(Mem, MemSize) ->
  ok = emmap:pwrite(Mem, 0, <<MemSize:32/integer>>).

reserve_tail(Mem, Tail) when is_integer(Tail) ->
  ok = emmap:pwrite(Mem, 12, <<Tail:32/integer>>);
reserve_tail(Mem, Tail) when byte_size(Tail) == 4 ->
  ok = emmap:pwrite(Mem, 12, Tail).

commit_tail(Mem, Tail) when byte_size(Tail) == 4 ->
  ok = emmap:pwrite(Mem, 8, Tail).

init_header(Mem) ->
  ok = emmap:pwrite(Mem, 4, ?INIT_SEGM_HEADER).

%%------------------------------------------------------------------------------
%% Multi-segment gen_server support
%%------------------------------------------------------------------------------

-ifdef(EUNIT).

%% Single-producer-single-consumer
spsc_queue_test() ->
  Filename  = "/tmp/queue1.bin",
  {ok, Mem} = open(Filename, 1024, [auto_unlink]),  %% Automatically delete file
  ?assert(filelib:is_regular(Filename)),
  ?assertEqual(nil, peek_front(Mem)),
  ?assertEqual(nil, peek_back(Mem)),
  % Enqueue data
  ?assertEqual(ok,  push(Mem, a)),
  ?assertEqual(a,   peek_front(Mem)),
  ?assertEqual(a,   peek_back(Mem)),

  ?assertEqual(ok,  push(Mem, b)),
  ?assertEqual(a,   peek_front(Mem)),
  ?assertEqual(b,   peek_back(Mem)),

  ?assertEqual([], [R || R <- [push(Mem, I) || I <- [c,1,2,3]], R /= ok]),

  ?assertEqual([a,b,c,1,2,3], lists:reverse(peek(Mem, [], fun(I,S) -> {cont, [I | S]} end))),

  ?assertEqual({16,106,106,1024}, header(Mem)),
  ?assertEqual(6, length(Mem)),

  % Dequeue data
  ?assertMatch({a, false}, read_head(Mem, true)),
  ?assertEqual(b,          pop(Mem)),
  ?assertEqual(c,          pop(Mem)),
  ?assertEqual(1,          pop(Mem)),
  ?assertMatch(2,          peek_front(Mem)),
  ?assertMatch(2,          try_pop(Mem, fun(2) -> 2 end)),
  ?assertError(failed,     try_pop(Mem, fun(3) -> erlang:error(failed) end)),
  ?assertMatch({3, true},  read_head(Mem, true)),
  ?assertEqual(nil,        pop(Mem)),
  ?assertEqual({106,106,106,1024}, header(Mem)),
  ?assert(purge(Mem)),
  ?assert(is_empty(Mem)),
  
  ?assertEqual([], [R || R <- [push(Mem, I) || I <- [a,b,1,2]], R /= ok]),
  ?assertEqual([a,b,1,2], lists:reverse(pop(Mem, [], fun(I,S) -> {cont, [I | S]} end))),
  ?assert(is_empty(Mem)),
  Term = string:copies("x", 128),
  ?assertEqual(ok,        push(Mem, Term, 9)),
  ?assertEqual(ok,        push(Mem, Term, 6)),
  ?assertEqual(ok,        push(Mem, Term, 1)),
  ?assertEqual([Term, Term, Term], lists:reverse(pop(Mem, [], fun(I,S) -> {cont, [I | S]} end))),
  ?assertEqual(ok, flush(Mem)).

file_size()             -> file_size(element(2, os:type())).
file_size(darwin)       -> 2048;
file_size(_)            -> 512.

check_size(Sz)          -> check_size(element(2,os:type()), Sz).
check_size(darwin, _Sz) -> 2048;
check_size(_,       Sz) -> Sz.

gen_server_queue_test() ->
  Filename  = "/tmp/queue2.bin",
  {ok, Pid} = start_link(?MODULE, Filename, file_size(), [auto_unlink]),
  ok = enqueue(Pid,  a),
  ?assert(filelib:is_regular(Filename)),
  ok = enqueue(Pid,  b),
  ok = enqueue(Pid,  c),

  ?assertEqual(a,   dequeue(Pid)),
  ?assertEqual(b,   inspect(Pid)),
  ?assertEqual(b,   dequeue(Pid)),
  ?assertError(failed, try_dequeue(Pid, fun(c) -> erlang:error(failed) end)),
  ?assertEqual(c,   try_dequeue(Pid, fun(c) -> c end)),
  ?assertEqual(nil, dequeue(Pid)),

  Blob = list_to_binary(string:copies("x", 256)),
  ?assertEqual(ok, enqueue(Pid, {1, Blob})),
  ?assertEqual(#{size => check_size(512), head => 16,next_tail => 292,tail => 292}, info(Pid)),
  ?assertEqual(ok, enqueue(Pid, {2, Blob})),
  ?assertEqual(#{size => check_size(1024),head => 16,next_tail => 568,tail => 568}, info(Pid)),
  ?assertEqual(ok, enqueue(Pid, {3, Blob})),
  ?assertEqual(#{size => check_size(1024),head => 16,next_tail => 844,tail => 844}, info(Pid)),
  ?assertEqual(ok, enqueue(Pid, {4, Blob})),
  ?assertEqual(#{size => 2048,head => 16,next_tail =>1120,tail =>1120}, info(Pid)),

  ?assertEqual({1,Blob},   dequeue(Pid)),
  ?assertEqual(#{size => 2048, head => 292,next_tail => 1120,tail => 1120}, info(Pid)),
  ?assertEqual({2,Blob},   dequeue(Pid)),
  ?assertEqual(#{size => 2048, head => 568,next_tail => 1120,tail => 1120}, info(Pid)),
  ?assertEqual({3,Blob},   dequeue(Pid)),
  ?assertEqual(#{size => 2048, head => 844,next_tail => 1120,tail => 1120}, info(Pid)),
  ?assertEqual({4,Blob},   dequeue(Pid)),
  ?assertEqual(#{size => 2048, head => 16,next_tail => 16,tail => 16}, info(Pid)),
  ?assertEqual(nil, dequeue(Pid)),
  
  case os:type() of
    {_, darwin} ->
      ?assertEqual({error, full}, enqueue(Pid, string:copies("x", 4096)));
    _ ->
      ok
  end.

-endif.
