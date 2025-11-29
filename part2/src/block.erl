-module(block).
-export([new/5, hash/1]).

%% Block creation + hashing

%% new(Number, MerkleRoot, BuilderAddr, PrevHash, TxIds) -> BlockMap
%% - Number: integer
%% - MerkleRoot: binary()
%% - BuilderAddr: string()
%% - PrevHash: binary() or <<"GENESIS">>
%% - TxIds: [integer()]
new(Number, MerkleRoot, BuilderAddr, PrevHash, TxIds) ->
    #{number      => Number,
      merkle_root => MerkleRoot,
      builder     => BuilderAddr,
      prev_hash   => PrevHash,
      tx_ids      => TxIds}.

%% hash(Block) -> BinaryHash
hash(Block) ->
    Number      = maps:get(number,      Block),
    MerkleRoot  = maps:get(merkle_root, Block),
    BuilderAddr = maps:get(builder,     Block),
    PrevHash    = maps:get(prev_hash,   Block),
    TxIds       = maps:get(tx_ids,      Block),

    %% Serialize deterministically
    Bin = io_lib:format("n:~p;mr:~p;builder:~s;prev:~p;txs:~p",
                        [Number, MerkleRoot, BuilderAddr, PrevHash, TxIds]),
    crypto:hash(sha256, list_to_binary(Bin)).
