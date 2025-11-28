-module(builder).
-export([start/2]).

%% start(NodePids, ValidTxs) -> spawn builder process
start(NodePids, ValidTxs) ->
    spawn(?MODULE, loop, [NodePids, ValidTxs, 1, <<"GENESIS">>]).

%% loop(NodePids, TxPool, BlockNum, PrevHash)
loop(_NodePids, [], _BlockNum, _PrevHash) ->
    io:format("Builder: no more transactions, done.~n"),
    ok;
loop(NodePids, TxPool, BlockNum, PrevHash) ->
    %% Take up to 10 transactions
    {BlockTxs, Remaining} = take_n(10, TxPool),

    %% Compute Merkle root
    MerkleRoot = merkle:root(BlockTxs),
    TxIds = [maps:get(id, Tx) || Tx <- BlockTxs],

    %% Create block
    Block = block:new(BlockNum, MerkleRoot, "Builder_1", PrevHash, TxIds),
    BlockHash = block:hash(Block),

    %% Broadcast to all nodes
    [Pid ! {block, Block, BlockHash} || Pid <- NodePids],
    io:format("Builder: published block ~p with ~p txs.~n",
              [BlockNum, length(BlockTxs)]),

    %% Next block: increment number, use current hash as prev_hash
    loop(NodePids, Remaining, BlockNum + 1, BlockHash).

%% Helper: take first N elements from list, return {Taken, Rest}
take_n(N, List) ->
    case length(List) =< N of
        true  -> {List, []};
        false -> lists:split(N, List)
    end.
