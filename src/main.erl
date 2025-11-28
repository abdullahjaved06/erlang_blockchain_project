-module(main).
-export([start/0]).

-define(TX_FILE, "../transactions.csv").

%% Simple driver to test tx + merkle + block

start() ->
    io:format("== Mini test run ==~n"),

    %% 1) Load transactions from CSV
    {ok, Lines} = read_lines(?TX_FILE),
    TxsWithId = make_txs(Lines, 1),

    io:format("Loaded ~p transactions from CSV.~n", [length(TxsWithId)]),

    %% 2) Filter valid transactions
    ValidTxs = [Tx || Tx <- TxsWithId, tx:is_valid(Tx)],
    io:format("Valid transactions: ~p~n", [length(ValidTxs)]),

    %% 3) Take up to 10 tx for first block
    {BlockTxs, _Rest} = lists:split(min(10, length(ValidTxs)), ValidTxs),

    %% 4) Compute Merkle root
    MerkleRoot = merkle:root(BlockTxs),

    %% 5) Build Block #1
    TxIds = [maps:get(id, Tx) || Tx <- BlockTxs],
    PrevHash = <<"GENESIS">>,
    Block = block:new(1, MerkleRoot, "Builder_1", PrevHash, TxIds),
    BlockHash = block:hash(Block),

    io:format("Block #1: ~p~n", [Block]),
    io:format("Block #1 hash: ~p~n", [BlockHash]),
    ok.

%% Helper: read file into list of lines
read_lines(Filename) ->
    case file:read_file(Filename) of
        {ok, Bin} ->
            %% Split by newline
            Lines = string:split(binary_to_list(Bin), "\n", all),
            %% Remove empty lines at the end
            {ok, [L || L <- Lines, L =/= ""]};
        Error ->
            Error
    end.

%% Turn [Line1, Line2, ...] into [Tx1, Tx2, ...] with ids
make_txs([], _Id) ->
    [];
make_txs([Line | Rest], Id) ->
    Tx = tx:parse_csv_line(Line, Id),
    [Tx | make_txs(Rest, Id+1)].
