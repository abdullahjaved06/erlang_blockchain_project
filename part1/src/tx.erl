-module(tx).
-export([parse_csv_line/2, is_valid/1]).

%% parse_csv_line(Line, Id) -> Map
%% Line is something like: "from_addr,to_addr,10"
parse_csv_line(Line, Id) ->
    %% Remove trailing newline
    Stripped = string:trim(Line),
    case string:split(Stripped, ",", all) of
        [From, To, AmountStr] ->
            #{id     => Id,
              from   => string:trim(From),
              to     => string:trim(To),
              amount => string:trim(AmountStr)};
        _Other ->
            %% If the line is malformed, mark it as obviously invalid
            #{id     => Id,
              from   => "",
              to     => "",
              amount => ""}
    end.

%% Transaction validity (simple version from spec):
%% - from, to, amount must be non-empty strings.
is_valid(Tx) ->
    From   = maps:get(from,   Tx, ""),
    To     = maps:get(to,     Tx, ""),
    Amount = maps:get(amount, Tx, ""),
    (From =/= "") andalso
    (To =/= "") andalso
    (Amount =/= "").
