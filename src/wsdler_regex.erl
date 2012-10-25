-module(wsdler_regex).
-compile(export_all). % For now

-include_lib("triq/include/triq.hrl").

-type(atomic_regex() ::
          {literal, char()}
        | {character_range, char(), char()} % Inclusive
        ).
-type(regex() ::
          atomic_regex()
        | {concat, [regex()]}
        | {choice, [regex()]}
        | {repeat, integer(), integer()|infinity} % min, max
        ).

-type(regex_token() ::
       atomic_regex()
       | begin_group | end_group | choice_separator
       | {repeat, integer(), integer()|infinity}
       ).

from_string(Str) ->
    case tokens(Str) of
        {ok,Tokens} -> tokens_to_tree(Tokens);
        {error,_}=Err -> Err
    end.


%%% Parsing step 1: atomic parts and delimiters
-spec tokens/1 :: (string()) -> {ok, [regex_token()]} .% | {error,_}.
tokens(Str) -> tokens(Str, []).
tokens(Str, Acc) ->
    case first_token(Str) of
        {error, Reason} -> {error, [{characters_left, length(Str)},
                                    {reason, Reason}]};
        {Token,Rest} -> tokens(Rest, [Token|Acc]);
        eof -> {ok,lists:reverse(Acc)}
    end.

-spec first_token/1 :: (string()) ->
                               eof | {regex_token(), string()} | {error, _}.
first_token("")        -> eof;
first_token("("++Rest) -> {begin_group, Rest};
first_token(")"++Rest) -> {end_group, Rest};
first_token("?"++Rest) -> {{repeat,0,1}, Rest};
first_token("*"++Rest) -> {{repeat,0,infinity}, Rest};
first_token("+"++Rest) -> {{repeat,1,infinity}, Rest};
first_token("|"++Rest) -> {choice_separator, Rest};
first_token("\\d"++Rest) -> {{character_range, $0, $9}, Rest};
first_token("{"++Rest) ->
    {N,M,Rest2} = parse_repeat_operator(Rest),
    {{repeat, N,M}, Rest2};
%% TODO: Other escape sequences
first_token("\\"++[C|Rest]) -> {{literal, C}, Rest};
first_token("\\")           -> {error, unfinished_escape_sequence};
%% TODO: Other special characters
first_token([C|Rest])       -> {{literal, C}, Rest}.


parse_repeat_operator(S) ->
    case lists:splitwith(fun is_digit/1, S) of
        {"",","++S2} ->                         % N is absent: N=0.
            parse_repeat_operator2(S2,0);
        {NStr,"}"++S2} ->                       % N is present, M is same
            N = M = list_to_integer(NStr),
            {N,M, S2};
        {NStr,","++S2} ->                       % N is present, M is separate
            parse_repeat_operator2(S2,list_to_integer(NStr))
    end.

parse_repeat_operator2(S, N) ->
    case lists:splitwith(fun is_digit/1, S) of
        {"","}"++Rest}    -> {N,infinity, Rest};
        {MStr,"}"++Rest} ->  {N,list_to_integer(MStr), Rest};
        _ -> error('expected_}')                % TODO: Make error handling more consistent
    end.

is_digit(C) -> C >= $0 andalso C =< $9.

%%% Parsing step 2: building the parse tree
tokens_to_tree(Tokens) ->
    Tokens2 = handle_grouping(Tokens),
    Choices = handle_alternation(Tokens2),
    make_choice([handle_concatenation_and_repetition(X) || X <- Choices]).

handle_grouping([begin_group | Rest]) ->
    case lists:splitwith(fun(T) -> T/=end_group end, Rest) of
        {Group, [end_group|Rest2]} ->
            [tokens_to_tree(Group) | handle_grouping(Rest2)];
        {_GroupTree, []} ->
            error('missing_)')
    end;
handle_grouping([]) -> [];
handle_grouping([X | Rest]) ->
    [X | handle_grouping(Rest)].

handle_alternation(L) -> handle_alternation(L, []).
handle_alternation(L, Acc) ->
    case lists:splitwith(fun(T) -> T/=choice_separator end, L) of
        {Choice, [choice_separator | Rest]} ->
            handle_alternation(Rest, [Choice|Acc]);
        {Choice, []} ->
            lists:reverse([Choice|Acc])
    end.

handle_concatenation_and_repetition(L) ->
    handle_concatenation_and_repetition(L, []).

handle_concatenation_and_repetition([], Acc) ->
    make_concat(lists:reverse(Acc));
handle_concatenation_and_repetition([{repeat,_,_} | _], _Acc) ->
    error('malplaced_repetition');              %TODO: handle errors better
handle_concatenation_and_repetition([_, {repeat,_,_}, {repeat,_,_} | _], _Acc) ->
    error('duplicated_repetition');              %TODO: handle errors better
handle_concatenation_and_repetition([X, {repeat,N,M} | Rest], Acc) ->
    Y = {repeat, X, N, M},
    handle_concatenation_and_repetition(Rest, [Y | Acc]);
handle_concatenation_and_repetition([X | Rest], Acc) ->
    handle_concatenation_and_repetition(Rest, [X | Acc]).

%%%

expand_repetition(X, N,M) ->
    if N==1, M==1 ->
            X;
       N==1, M==infinity ->
            {repeat, X};
       N==0 ->
            make_choice([make_empty(), expand_repetition(X, 1,M)]);
       M<N ->
            error({bad_repetition, N, M});
       N>0 ->
            make_concat([X, expand_repetition(X, N-1, safe_minus_one(M))])
    end.

safe_minus_one(infinity) -> infinity;
safe_minus_one(M) when is_integer(M) -> M-1.

%%%

make_concat([X]) -> X;
make_concat(L) when is_list(L) -> {concat, L}.

make_choice([X]) -> X;
make_choice(L) when is_list(L) -> {choice, L}.

make_empty() -> {concat, []}.



-spec to_generator/1 :: (regex()) -> _. % triq_dom:dom_rec(), really.
to_generator(Regex) ->
    {Gen,_Size}=Tmp = to_generator_and_size(Regex),
    ?LET(Str, Gen, lists:flatten(Str)).

to_generator_and_size({literal,C}) -> {[C],1};
to_generator_and_size({character_range,C1,C2}) ->
    Size = round(0.5 + math:sqrt(C2-C1)),
    {[triq_dom:choose(C1,C2)], Size};
to_generator_and_size({concat, Rs}) ->
    {Gens, SizeSum} =
        lists:mapfoldl(fun (R,AccSz) ->
                               {G,Sz} = to_generator_and_size(R),
                               {G, Sz+AccSz}
                       end,
                    0, Rs),
    Size = round(0.5 + math:sqrt(SizeSum)),
    {Gens, Size};
to_generator_and_size({choice, Rs}) ->
    {FreqSpec, SizeSum} =
        lists:mapfoldl(fun (R,AccSz) ->
                               {G,Sz} = to_generator_and_size(R),
                               {{Sz,G}, Sz+AccSz}
                       end,
                       0, Rs),
    Size = round(0.5 + math:sqrt(SizeSum)),
    {triq_dom:frequency(FreqSpec), Size};
to_generator_and_size({repeat, ElmRegex, Min, Max}) ->
    {ElmGen, ElmSize} = to_generator_and_size(ElmRegex),
    case Max of
        infinity ->
            Size = 4 * ElmSize,
            Gen = lists:duplicate(Min,ElmGen) ++ triq_dom:list(ElmGen);
        _ ->
            %% TODO: triq doesn't support "list of length <lengthspec>". This is a workaround:
            Size = round(0.5 + math:sqrt(Max-Min)) * ElmSize,
            {Gen,_} = to_generator_and_size(expand_repetition(ElmRegex, Min, Max))
%%             Gen =?SIZED(Size,
%%                         ?LET(Length, choose(Min,Max),
%%                              lists:unzip([triq_dom:pick(ElmGen, Size)
%%                                           || _ <- lists:seq(1,Length)])
%%                             )
%%                        )
    end,
    {Gen, Size}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

empty_parse_test() ->
    ?assertEqual({concat,[]}, from_string("")).

simple_character_test() ->
    ?assertEqual({literal, $X}, from_string("X")).

special_characters_test() ->
    ?assertEqual({literal, $\\}, from_string("\\\\")),
    ?assertEqual({literal, $|}, from_string("\\|")),
    ?assertEqual({literal, $(}, from_string("\\(")),
    ?assertEqual({literal, $)}, from_string("\\)")),
    ?assertEqual({literal, $[}, from_string("\\[")),
    ?assertEqual({literal, $]}, from_string("\\]")),
    ?assertEqual({literal, ${}, from_string("\\{")),
    ?assertEqual({literal, $}}, from_string("\\}")),
    ?assertEqual({literal, $?}, from_string("\\?")),
    ?assertEqual({literal, $*}, from_string("\\*")),
    ?assertEqual({literal, $+}, from_string("\\+")),
    ok.

simple_concat_test() ->
    ?assertEqual({concat,[{literal, $x}, {literal, $y}]}, from_string("xy")),
    ?assertEqual({concat,[{literal, $x}, {literal, $y}, {literal, $z}]},
             from_string("xyz")),
    ok.

repetition_test() ->
    ?assertEqual({repeat, {literal, $x}, 0,infinity}, from_string("x*")),
    ?assertEqual({repeat, {literal, $x}, 1,infinity}, from_string("x+")),
    ?assertEqual({repeat, {literal, $x}, 0,1}, from_string("x?")),

    ?assertEqual({repeat, {literal, $x}, 5,5}, from_string("x{5,5}")),
    ?assertEqual({repeat, {literal, $x}, 125,125}, from_string("x{125}")),
    ?assertEqual({repeat, {literal, $x}, 0,5}, from_string("x{,5}")),
    ?assertEqual({repeat, {literal, $x}, 5,infinity}, from_string("x{5,}")),
    ?assertEqual({repeat, {literal, $x}, 12,345}, from_string("x{12,345}")),
    ok.

choice_test() ->
    ?assertEqual({choice, [{literal, $x}, {literal, $y}]},
                 from_string("x|y")),
    ?assertEqual({choice, [{literal, $x}, {concat,[]}]},
                 from_string("x|")),
    ?assertEqual({choice, [{concat,[]}, {literal, $y}]},
                 from_string("|y")),
    ok.

precedence_concat_vs_choice_test() ->
    %% Concatenation binds stronger than alternation.
    X = {literal, $x},
    Y = {literal, $y},
    Z = {literal, $z},
    W = {literal, $w},
    ?assertEqual({choice, [{concat, [X,Y]}, {concat, [W,Z]}]},
                 from_string("xy|wz")),
    ?assertEqual({choice, [X, {concat, [W,Z]}]},
                 from_string("x|wz")),
    ?assertEqual({choice, [{concat, [X,Y]}, Z]},
                 from_string("xy|z")),

    %% Grouping overrides:
    ?assertEqual({concat, [X,{choice, [Y,W]},Z]},
                 from_string("x(y|w)z")),
    ?assertEqual({concat, [{choice, [X,W]},Z]},
                 from_string("(x|w)z")),
    ?assertEqual({concat, [X,{choice, [Y,Z]}]},
                 from_string("x(y|z)")),

    ok.

-endif.
