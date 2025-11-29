-module(merkle).
-export([root/1]).

%% Build a Merkle root from a list of transaction maps

%% Public API
%% root(TxList) -> BinaryHash
%% TxList is a list of maps, each with at least an id field (or any fields).
root([]) ->
    %% Empty tree case: return some fixed value
    crypto:hash(sha256, <<"EMPTY">>);
root(TxList) ->
    %% Step 1: compute leaf hashes
    Leaves = [leaf_hash(Tx) || Tx <- TxList],
    build_root(Leaves).

%% Compute hash of a transaction map
leaf_hash(Tx) ->
    %% Serialize Tx in some deterministic way
    %% Example: <<"id:1;from:...;to:...;amount:...">>
    Id     = maps:get(id,     Tx, 0),
    From   = maps:get(from,   Tx, <<"">>),
    To     = maps:get(to,     Tx, <<"">>),
    Amount = maps:get(amount, Tx, <<"">>),

    %% Convert to binaries
    Bin = io_lib:format("id:~p;from:~s;to:~s;amount:~s",
                        [Id, From, To, Amount]),
    crypto:hash(sha256, list_to_binary(Bin)).

%% Build the Merkle root from a list of hashes (binaries)
build_root([Single]) ->
    Single;
build_root(Hashes) ->
    Pairs = pair_up(Hashes),
    Parents = [parent_hash(H1, H2) || {H1, H2} <- Pairs],
    build_root(Parents).

%% Pair up list elements, duplicating last if odd length
pair_up([]) ->
    [];
pair_up([H]) ->
    [{H, H}];  %% duplicate lone hash
pair_up([H1, H2 | Rest]) ->
    [{H1, H2} | pair_up(Rest)].

parent_hash(H1, H2) ->
    crypto:hash(sha256, <<H1/binary, H2/binary>>).
