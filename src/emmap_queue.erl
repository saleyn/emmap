%%%-----------------------------------------------------------------------------
%%% @doc     Persistent FIFO queue
%%%          The FIFO queue can be used as a persistent container of messages
%%%          with constant time push and pop operations.  Additionally, this
%%%          module provides a gen_server API, which wraps the queue for use
%%%          in multi-process applications.
%%%          The queue is stored in a memory-mapped file, and it automatically
%%%          grows if the messages are not dequeued from the queue.
%%%          See test cases at the end of this module for sample use cases.
%%% @author  Serge Aleynikov
%%% @end
%%%-----------------------------------------------------------------------------
%%% Created: 2021-12-10
%%%-----------------------------------------------------------------------------
-module(emmap_queue).
-export([open/3, close/1]).
-export([purge_queue/1, is_empty/1, length/1, push/2, pop/1, peek/1, try_pop/2,
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
  Msg = peek(Mem),
  {reply, Msg, State};

handle_call({try_pop, Fun}, _From, #state{mem=Mem} = State) ->
  try
    {reply, {ok, try_pop_and_purge(Mem, Fun)}, State}
  catch _:E:ST ->
    {reply, {error, E, ST}, State}
  end;

handle_call(info, _From, #state{mem=Mem} = State) ->
  case header(Mem) of
    {Head, Tail, NextTail, SegmSize} ->
      {reply, #{head => Head, tail => Tail, next_tail => NextTail, size => SegmSize}, State};
    Error ->
      {reply, Error, State}
  end.

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
          ok = emmap:pwrite(Mem, 0, <<SegmSize:32/integer>>),
          {ok, Mem};
        {_Head, Tail, Tail, _SegmSize} ->
          {ok, Mem};
        {_Head, Tail, _NextTail, _SegmSize} ->
          % Need to repair last uncommitted tail
          ok = emmap:pwrite(Mem, 12, <<Tail:32/integer>>),
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

%% @doc Purge queue.  It is a constant time operation.
purge_queue(Mem) ->
  case is_empty_queue(Mem) of
    {true, 0} ->
      true;
    {true, _} ->
      ok = emmap:pwrite(Mem, 4, ?INIT_SEGM_HEADER),
      true;
    false ->
      false
  end.

header(Mem) ->
  case emmap:pread(Mem, 0, 16) of
    {ok, <<SegmSize:32/integer, Head:32/integer, Tail:32/integer, NextTail:32/integer>>} ->
      {Head, Tail, NextTail, SegmSize};
    Error ->
      Error
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
  Bin = term_to_binary(Term),
  Sz0 = byte_size(Bin)+8,
  Pad = Sz0 rem 8,
  Sz  = Sz0 + Pad,
  Msg = <<Sz:32/integer, Bin/binary, 0:(Pad*8)/integer, Sz:32/integer>>,
  Arg = header(Mem),
  (fun
    G({_Head,_Tail, NextTail, SegmSize}) when NextTail+Sz > SegmSize ->
      case emmap:resize(Mem) of
        {ok, NewSegmSize} ->
          ok = emmap:pwrite(Mem, 0, <<NewSegmSize:32/integer>>),
          G(header(Mem));
        {error, fixed_size} ->
          {error, full};    % Queue is full and was given fixed_size option when opened
        {error, Reason} ->
          {error, Reason}
      end;
    G({_Head, Tail, NextTail,_SegmSize}) ->
      NewTail = NextTail+Sz,
      TailBin = <<NewTail:32/integer>>,
      ok = emmap:pwrite(Mem, 12,   TailBin), % Increment next tail offset
      ok = emmap:pwrite(Mem, Tail, Msg),     % Write message
      ok = emmap:pwrite(Mem, 8,    TailBin); % Commit tail offset
    G(Error) ->
       Error
  end)(Arg).

%% @doc Pop a term from the queue. This function has a constant-time complexity.
pop(Mem) ->
  case pop_internal(Mem, true) of
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
  case pop_internal(Mem, Fun) of
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
  case pop_internal(Mem, Fun) of
    {Res, true} ->
      ok = emmap:pwrite(Mem, 4, ?INIT_SEGM_HEADER),
      Res;
    {Res, false} ->
      Res;
    nil ->
      nil
  end.

%% @doc Peek the next term from the queue without popping it.
%% This function has a constant-time complexity.
peek(Mem) ->
  case pop_internal(Mem, false) of
    {Msg, _IsEnd} ->
      Msg;
    nil ->
      nil
  end.

%% @doc Pop a term from the queue and reclaim queue's memory if the queue is empty.
%% This function has a constant-time complexity.
pop_and_purge(Mem) ->
  case pop_internal(Mem, true) of
    {Msg, true} ->
      ok = emmap:pwrite(Mem, 4, ?INIT_SEGM_HEADER),
      Msg;
    {Msg, false} ->
      Msg;
    nil ->
      nil
  end.

pop_internal(Mem, Pop) ->
  case header(Mem) of
    {Head,  Tail, NextTail, _SegmSize} when Head < Tail, Head >= ?HEADER_SZ, Tail >= ?HEADER_SZ ->
      {ok,   <<Sz:32/integer>>} = emmap:pread(Mem, Head, 4),
      BinSz  = Sz-8,
      {ok,   <<Bin:BinSz/binary, EndSz:32/integer>>} = emmap:pread(Mem, Head+4, BinSz+4),
      EndSz /= Sz   andalso erlang:error({message_size_mismatch, {Sz, EndSz}, Head}),
      NextHd = Head+Sz,
      Msg    = binary_to_term(Bin),
      IsEnd  = (NextHd =:= Tail) andalso (NextHd =:= NextTail),
      case Pop of
        true ->
          ok = emmap:pwrite(Mem, 4, <<NextHd:32/integer>>),
          {Msg, IsEnd};
        false ->
          {Msg, IsEnd};
        _ when is_function(Pop, 1) ->
          Res = Pop(Msg),
          ok = emmap:pwrite(Mem, 4, <<NextHd:32/integer>>),
          {Res, IsEnd}
      end;
    {Head, Tail, _NextTail, _} when Head >= ?HEADER_SZ, Tail >= ?HEADER_SZ ->
      nil;
    {_Head, _Tail, _NextTail, _} = H ->
      erlang:error({invalid_queue_header, H});
    {error, Error} ->
      erlang:error(Error)
  end.

%%------------------------------------------------------------------------------
%% Multi-segment gen_server support
%%------------------------------------------------------------------------------

-ifdef(EUNIT).

%% Single-producer-single-consumer
spsc_queue_test() ->
  Filename  = "/tmp/queue1.bin",
  {ok, Mem} = open(Filename, 1024, [auto_unlink]),  %% Automatically delete file
  ?assert(filelib:is_regular(Filename)),
  % Enqueue data
  [?assertEqual(ok, push(Mem, I)) || I <- [a,b,c,1,2,3]],

  ?assertEqual({16,112,112,1024}, header(Mem)),
  ?assertEqual(6, length(Mem)),

  % Dequeue data
  ?assertMatch({a, false}, pop_internal(Mem, true)),
  ?assertEqual(b,          pop(Mem)),
  ?assertEqual(c,          pop(Mem)),
  ?assertEqual(1,          pop(Mem)),
  ?assertMatch(2,          peek(Mem)),
  ?assertMatch(2,          try_pop(Mem, fun(2) -> 2 end)),
  ?assertError(failed,     try_pop(Mem, fun(3) -> erlang:error(failed) end)),
  ?assertMatch({3, true},  pop_internal(Mem, true)),
  ?assertEqual(nil,        pop(Mem)),
  ?assertEqual({112,112,112,1024}, header(Mem)),
  ?assert(purge_queue(Mem)),
  ?assert(is_empty(Mem)).

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
