-module(wsdler_generators).

-compile(export_all).

-include("wsdler.hrl").
-include_lib("triq/include/triq.hrl").

generator(#simpleRestriction{enumeration=Enum}) when Enum /= [] ->
    oneof([return(X) || X <- Enum]);
generator(#simpleRestriction{base="boolean"}) ->
    oneof(["false","true","0","1"]);
generator(#simpleRestriction{base="dateTime"}) ->
    %% TODO: Obey facets: min/max-Inclusive/Exclusive, and pattern
    ?LET(X, resize(5.0,real()),
         format_years_from_now_as_datetime(math:pow(X,7)));
generator(#simpleRestriction{base="string", pattern=Pattern}) when Pattern /= undefined ->
    %% TODO: Obey facets: min/max-Inclusive/Exclusive, and minLength/maxLength
    Statem = regex_to_statemachine(Pattern),
    statemachine_to_generator(Statem);
generator(#simpleRestriction{base="string", minLength=MinLen0, maxLength=MaxLen}) when MinLen0 /= undefined; MaxLen /= undefined ->
    MinLen = if MinLen0==undefined -> 0;
                true -> MinLen0
             end,
    CharGen = frequency([{20,char()}, {2,choose(128,255)}, {1,unicode_char()}]),
    if MaxLen==undefined ->
            ?LET(X, int(),
                 [CharGen || _ <- lists:seq(1,MinLen+abs(X))]);
       true ->
            ?LET(X, choose(MinLen,MaxLen),
                 [CharGen || _ <- lists:seq(1,X)])
    end;
generator(_Other) ->
    'TODO'.

%%%========== DATETIME Generation ========================================

format_years_from_now_as_datetime(YearsFromNow) ->
    Now = round(now_gregorian_seconds() + YearsFromNow*(365*24*60*60)),
    {{Y,Mo,D},{H,Mi,S}} = calendar:gregorian_seconds_to_datetime(Now),
    [] ++
        format_int(Y,4)  ++ [$-] ++
        format_int(Mo,2) ++ [$-] ++
        format_int(D,2) ++ [$T] ++
        format_int(H,2) ++ [$:] ++
        format_int(Mi,2) ++ [$:] ++
        format_int(S,2). % TODO: Add decimals and optional timezone


now_gregorian_seconds() ->
    {Megas,Secs,Micros} = os:timestamp(),
    S = Megas * 1000000 + Secs + Micros * 1.0e-6,
    GS = S + (1970*365+478)*24*60*60,
    GS.

format_int(X,M) -> format_int(X,M,[]).
format_int(_X,0,Acc) -> Acc;
format_int(X,N,Acc) -> format_int(X div 10, N-1, [$0 + X rem 10 | Acc]).

%%%========== STRING Generation ========================================

regex_to_statemachine(Regex) -> regex_to_statemachine(Regex,[]).

%% TODO: This list-based model is too simple; handle combinator and precedences properly. (A list-to-tree post-processing step will probably do.)
regex_to_statemachine([], Acc) ->
    lists:reverse(Acc,[accept]);
regex_to_statemachine("\\d"++Rest, Acc) ->
    regex_to_statemachine(Rest, [{char_range, $0, $9} | Acc]);
regex_to_statemachine(("{"++Rest1)=Regex, [Last|Acc]) ->
    case lists:splitwith(fun(C)-> $0 =< C andalso C =< $9 end, Rest1) of
        {CountStr, "}"++Rest2} when CountStr /= "" ->
            Count = list_to_integer(CountStr),
            regex_to_statemachine(Rest2, lists:duplicate(Count,Last)++Acc);
        _ ->
            error({bad_regex, Regex})
    end.

statemachine_to_generator([accept]) -> "";
statemachine_to_generator([{char_range,Start,End} | Rest]) ->
    TailGen = statemachine_to_generator(Rest),
    [choose(Start,End) | TailGen].
