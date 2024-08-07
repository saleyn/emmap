-module(block_storage_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    % open underlying memory-mapped file
    {ok, MFile, #{size := 1}} = emmap:open("/tmp/simple.bin", 0, 1, [create, write, shared]),

    % init block storage of the fixed block size
    ok = emmap:init_block_storage(MFile, 1),

    % storing block of a wrong size is an error
    ?assertError(badarg, emmap:store_block(MFile, <<1, 2, 3>>)),

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

    Addrs_ = [Addr || {Addr, _Data} <- L1],
    ?assertMatch(Addrs_, Addrs),

    % remove all blocks
    lists:foreach(fun (Addr) ->
        ?assertMatch(true, emmap:free_block(MFile, Addr))
    end, Addrs),

    % ensure stogare is empty now
    ?assertMatch([], emmap:read_blocks(MFile)),

    ok.

sustainability_test() ->
    FName = "/tmp/garbage.bin",
    lists:foreach(fun (Size) ->
        ok = file:write_file(FName, rand:bytes(Size)),
        {ok, MFile, #{size := Size}} = emmap:open(FName, []),
        L = try
            emmap:read_blocks(MFile)
        catch error:badarg ->
            []
        end,
        ?assert(is_list(L))
    end, lists:seq(1, 1000)).

big_random_test() ->
    FileName = "/tmp/bigrandom.bin",
    BlockSize = 1531,
    Iterations = 100_000,
    MaxSize = 200,

    {ok, MFile, #{size := 1}} = emmap:open(FileName, 0, 1, [create, write, shared]),
    ok = emmap:init_block_storage(MFile, BlockSize),

    Acc = loop(Iterations, MFile, #{}, fun
        (data) ->
            rand:bytes(BlockSize);
        (Map) ->
            map_size(Map) < rand:uniform(MaxSize)
    end),

    ?assertMatch(ok, emmap:close(MFile)),

    {ok, MFile_, _} = emmap:open(FileName, [write, shared]),

    loop(Iterations, MFile_, Acc, fun
        (data) ->
            rand:bytes(BlockSize);
        (Map) ->
            map_size(Map) < rand:uniform(MaxSize)
    end),

    ok.

loop(0, _, Map, _) ->
    Map;
loop(N, MFile, Map, Fun) ->
    % ?debugFmt("~p items", [map_size(Map)]),
    case Fun(Map) of
        true ->
            Data = Fun(data),
            Addr = emmap:store_block(MFile, Data),
            loop(N - 1, MFile, Map#{Addr => Data}, Fun);
        false ->
            Addr = lists:nth(rand:uniform(map_size(Map)), maps:keys(Map)),
            {Data, Map_} = maps:take(Addr, Map),
            Bytes = emmap:read_block(MFile, Addr),
            ?assertMatch(Data, Bytes),
            ?assertMatch(true, emmap:free_block(MFile, Addr)),
            loop(N - 1, MFile, Map_, Fun)
    end.

read_chunks(MFile, N) ->
    read_chunks(MFile, 0, N, []).

read_chunks(_MFile, eof, _, Acc) ->
    lists:concat(Acc);
read_chunks(MFile, Start, N, Acc) ->
    {L, Cont} = emmap:read_blocks(MFile, Start, N),
    ?assert(is_list(L)),
    read_chunks(MFile, Cont, N, [L | Acc]).

block_storage_test() ->
    {ok, MFile, #{size := 8}} = emmap:open("/tmp/storage.bin", 0, 8, [create, write, shared]),
    ok = emmap:init_block_storage(MFile, 8),
    write_n_blocks(4096, MFile, 8),

    {T1, L1} = timer:tc(fun () -> emmap:read_blocks(MFile) end),
    ?debugFmt("elapsed ~p us~n", [T1]),
    ?assert(is_list(L1)),
    ?assertMatch(4096, length(L1)),
    % ?debugFmt("result: ~p~n", [L1]),

    {T3, L3} = timer:tc(fun () -> read_chunks(MFile, 100) end),
    ?debugFmt("elapsed ~p us~n", [T3]),
    ?assert(is_list(L3)),
    ?assertMatch(4096, length(L3)),

    ?assertMatch(L1, L3),
    lists:foreach(fun ({Addr, _Data}) ->
        ?assertMatch(true, emmap:free_block(MFile, Addr))
    end, L1),

    ?assertMatch([], emmap:read_blocks(MFile)),

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
