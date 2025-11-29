-module(node).
-export([start_nonvalidator/1, loop/1]).

%% Start a non-validator node with a given Id
start_nonvalidator(Id) ->
    FileNameIo = io_lib:format("node_~p_blocks.csv", [Id]),
    LogFile = list_to_binary(FileNameIo),
    State = #{id => Id, log_file => LogFile},
    spawn(?MODULE, loop, [State]).

loop(State) ->
    receive
        {block, Block, BlockHash} ->
            log_block(State, Block, BlockHash),
            loop(State);

        stop ->
            io:format("Node ~p stopping.~n", [maps:get(id, State)]),
            ok;

        Other ->
            io:format("Node ~p received unknown message: ~p~n",
                      [maps:get(id, State), Other]),
            loop(State)
    end.

%% Append block info to this node's CSV log
log_block(State, Block, BlockHash) ->
    File = maps:get(log_file, State),
    Line = format_block_line(Block, BlockHash),
    %% append mode: write at end of file
    ok = file:write_file(File, Line, [append]).

format_block_line(Block, BlockHash) ->
    Number     = maps:get(number,      Block),
    Builder    = maps:get(builder,     Block),
    PrevHash   = maps:get(prev_hash,   Block),
    MerkleRoot = maps:get(merkle_root, Block),
    TxIds      = maps:get(tx_ids,      Block),

    %% CSV-style line: block_no,builder,prev_hash,merkle_root,tx_ids,block_hash
    IoLine = io_lib:format("~p,~s,~p,~p,~p,~p~n",
                           [Number, Builder, PrevHash, MerkleRoot, TxIds, BlockHash]),
    list_to_binary(IoLine).
