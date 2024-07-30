-module(block_storage_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    file:write_file("/tmp/testpipe", [os:getpid(), "\n"]),
    timer:sleep(100),

    {ok, MFile, #{size := 1}} = emmap:open("simple.bin", 0, 1, [create, write, shared]),
    ok = emmap:init_block_storage(MFile, 1),

    % write-read single blocks in a loop
    Addrs = lists:foldl(fun (N, Acc) ->
        Data = integer_to_binary(N),
        Addr = emmap:store_block(MFile, Data),
        Bytes = emmap:read_block(MFile, Addr),
        ?assertMatch(Data, Bytes),
        [Addr | Acc]
    end, [], lists:seq(0, 9)),

    % read all blocks
    L1 = emmap:read_blocks(MFile),
    ?assert(is_list(L1)),

    % read one big chunk
    {L2, eof} = emmap:read_blocks(MFile, 0, 100),
    ?assert(is_list(L2)),
    ?assertMatch(L1, L2),

    % read in chunks
    L3 = read_chunks(MFile, 3),
    ?assertMatch(L1, L3),

    % remove all blocks
    lists:foreach(fun (Addr) ->
        ?assertMatch(true, emmap:free_block(MFile, Addr))
    end, Addrs),

    % ensure stogare is empty now
    ?assertMatch([], emmap:read_blocks(MFile)),

    ok.

read_chunks(MFile, N) ->
    read_chunks(MFile, 0, N, []).

read_chunks(_MFile, eof, _, Acc) ->
    lists:concat(Acc);
read_chunks(MFile, Start, N, Acc) ->
    {L, Cont} = emmap:read_blocks(MFile, Start, N),
    ?assert(is_list(L)),
    read_chunks(MFile, Cont, N, [L | Acc]).

block_storage_test() ->
    {ok, MFile, #{size := 8}} = emmap:open("storage.bin", 0, 8, [create, write, shared]),
    ok = emmap:init_block_storage(MFile, 8),
    write_n_blocks(4096, MFile, 8),

    {T1, L1} = timer:tc(fun () -> emmap:read_blocks(MFile) end),
    ?debugFmt("elapsed ~p us~n", [T1]),
    ?assert(is_list(L1)),
    ?assertMatch(4096, length(L1)),
    % ?debugFmt("result: ~p~n", [L1]),

    {T2, {L2, Cont}} = timer:tc(fun () -> emmap:read_blocks(MFile, 0, 5) end),
    ?debugFmt("elapsed ~p us~n", [T2]),
    ?assert(is_list(L2)),
    ?assert(is_integer(Cont)),
    ?assertMatch(5, length(L2)),
    % ?debugFmt("result: ~p~n", [L2]),

    {T3, L3} = timer:tc(fun () -> read_chunks(MFile, 100) end),
    ?debugFmt("elapsed ~p us~n", [T3]),
    ?assert(is_list(L3)),
    ?assertMatch(4096, length(L3)),

    ok = emmap:flush(MFile),
    ok.

write_n_blocks(0, _, _) -> ok;
write_n_blocks(N, MFile, Size) ->
    Bytes1 = rand:bytes(Size),

    Addr = emmap:store_block(MFile, Bytes1),
    % io:format(user, "addr: ~p~n", [Addr]),
    ?assert(is_integer(Addr) andalso Addr >= 0),

    Bytes2 = emmap:read_block(MFile, Addr),
    ?assertMatch(Bytes1, Bytes2),

    write_n_blocks(N - 1, MFile, Size).
