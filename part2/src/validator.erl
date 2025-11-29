-module(validator).
-export([start/3, start_election/1]).

%% Start a validator with a given Id, the full validator list,
%% and the initial proposer group.
start(Id, ValidatorIds, ProposerGroup) ->
    spawn(fun() -> init(Id, ValidatorIds, ProposerGroup) end).

init(Id, ValidatorIds, ProposerGroup) ->
    %% Register this process as validator_1, validator_2, ...
    NameIo = io_lib:format("validator_~p", [Id]),
    NameAtom = list_to_atom(lists:flatten(NameIo)),
    register(NameAtom, self()),

    LogIo = io_lib:format("validator_~p_election.log", [Id]),
    LogFile = list_to_binary(LogIo),

    IsHead = (Id =:= hd(ProposerGroup)),

    State = #{
        id => Id,
        validators => ValidatorIds,
        proposer_group => ProposerGroup,
        log_file => LogFile,
        is_head => IsHead,
        election_start_time => undefined
    },

    log(State,
        io_lib:format("Bootstrap. Validators=~p, proposer_group=~p, is_head=~p~n",
                      [ValidatorIds, ProposerGroup, IsHead])),

    loop(State).

%% Public API: ask the (current) head to start an election
start_election(HeadId) ->
    NameIo = io_lib:format("validator_~p", [HeadId]),
    NameAtom = list_to_atom(lists:flatten(NameIo)),
    case whereis(NameAtom) of
        undefined ->
            {error, not_found};
        Pid ->
            Pid ! {begin_election},
            ok
    end.

%% ================== Main loop ==================
loop(State) ->
    receive
        {begin_election} ->
            NewState = handle_begin_election(State),
            loop(NewState);

        {shuffle_round, ShuffledList, HeadId} ->
            NewState = handle_shuffle_round(State, ShuffledList, HeadId),
            loop(NewState);

        {new_proposer_group, Group, OldHeadId, NewHeadId} ->
            NewState = handle_new_group(State, Group, OldHeadId, NewHeadId),
            loop(NewState);

        {become_epoch_head, Group, OldHeadId} ->
            NewState = handle_become_head(State, Group, OldHeadId),
            loop(NewState);

        {start_new_epoch, Group, HeadId} ->
            NewState = handle_start_new_epoch(State, Group, HeadId),
            loop(NewState);

        stop ->
            log(State, "Stopping validator\n"),
            ok;

        Other ->
            log(State, io_lib:format("Unknown message: ~p~n", [Other])),
            loop(State)
    end.

%% ================== Handlers ==================

%% Only the current head should really handle this.
handle_begin_election(State = #{id := Id, is_head := true}) ->
    log(State, "Election started (I am current head)\n"),

    ValidatorIds = maps:get(validators, State),
    StartTime = erlang:monotonic_time(millisecond),

    Shuffled = rand_shuffle(ValidatorIds),

    NextId = next_validator_id(Id, ValidatorIds),
    NextPid = validator_pid(NextId),
    log(State,
        io_lib:format("Sending initial shuffled list to validator ~p: ~p~n",
                      [NextId, Shuffled])),
    NextPid ! {shuffle_round, Shuffled, Id},

    State#{election_start_time => StartTime};

handle_begin_election(State) ->
    log(State, "Received begin_election but I'm not head, ignoring.~n"),
    State.

handle_shuffle_round(State = #{id := Id, validators := ValidatorIds},
                     ShuffledList, HeadId) ->
    log(State,
        io_lib:format("Received list in shuffle_round: ~p~n", [ShuffledList])),

    NewList = rand_shuffle(ShuffledList),

    if
        Id =:= HeadId ->
            handle_final_shuffle(State, NewList, HeadId);
        true ->
            NextId = next_validator_id(Id, ValidatorIds),
            NextPid = validator_pid(NextId),
            log(State,
                io_lib:format("Forwarding reshuffled list to validator ~p: ~p~n",
                              [NextId, NewList])),
            NextPid ! {shuffle_round, NewList, HeadId},
            State
    end.

handle_final_shuffle(State, FinalList, HeadId) ->
    log(State,
        io_lib:format("Head received final shuffled list: ~p~n", [FinalList])),

    Group = choose_proposer_group(FinalList),
    NewHeadId = hd(Group),

    EndTime = erlang:monotonic_time(millisecond),
    StartTime = maps:get(election_start_time, State, EndTime),
    Duration = EndTime - StartTime,

    log(State,
        io_lib:format(
          "Chose new proposer group: ~p (new head: ~p). Election time: ~p ms~n",
          [Group, NewHeadId, Duration])),

    %% Broadcast group to all validators
    [ validator_pid(VId) ! {new_proposer_group, Group, HeadId, NewHeadId}
      || VId <- maps:get(validators, State) ],

    %% Tell the new head to broadcast start of new epoch
    NewHeadPid = validator_pid(NewHeadId),
    NewHeadPid ! {become_epoch_head, Group, HeadId},

    State#{proposer_group => Group}.

handle_new_group(State, Group, OldHeadId, NewHeadId) ->
    log(State,
        io_lib:format(
          "Received new proposer group from ~p: ~p (new head: ~p)~n",
          [OldHeadId, Group, NewHeadId])),
    State#{proposer_group => Group}.

handle_become_head(State = #{id := Id}, Group, OldHeadId) ->
    log(State,
        io_lib:format(
          "I am the new head (old head was ~p). Broadcasting start of new epoch.~n",
          [OldHeadId])),

    %% Mark self as head
    NewState = State#{is_head => true, proposer_group => Group},

    %% Broadcast start of new epoch
    [ validator_pid(VId) ! {start_new_epoch, Group, Id}
      || VId <- maps:get(validators, NewState) ],

    NewState.

handle_start_new_epoch(State, Group, HeadId) ->
    log(State,
        io_lib:format(
          "Start new epoch. Head is validator ~p. Proposer group: ~p~n",
          [HeadId, Group])),
    State.

%% ================== Utility functions ==================

validator_pid(Id) ->
    NameIo = io_lib:format("validator_~p", [Id]),
    NameAtom = list_to_atom(lists:flatten(NameIo)),
    whereis(NameAtom).

%% Next validator in the ring; wrap around at the end
next_validator_id(Id, ValidatorIds) ->
    case lists:dropwhile(fun(X) -> X =/= Id end, ValidatorIds) of
        [_Current, Next | _] ->
            Next;
        [_Last] ->
            %% Wrap around
            hd(ValidatorIds);
        [] ->
            %% Should not happen if Id is in the list
            hd(ValidatorIds)
    end.

%% Choose top 10% of validators (at least 1)
choose_proposer_group(FinalList) ->
    Len = length(FinalList),
    RawSize = Len div 10,
    Size = case RawSize of
        0 -> 1;
        N -> N
    end,
    lists:sublist(FinalList, Size).

%% Random shuffle using rand
rand_shuffle(List) ->
    _ = rand:seed(exsplus,
                  {erlang:monotonic_time(),
                   erlang:phash2(self()),
                   erlang:unique_integer()}),
    Tagged = [{rand:uniform(), X} || X <- List],
    [X || {_K, X} <- lists:sort(Tagged)].

log(State, IoData) ->
    File = maps:get(log_file, State),
    Bin = iolist_to_binary(IoData),
    ok = file:write_file(File, Bin, [append]).
