-module(wsdler_regex).
-compile(export_all). % For now

-type(atomic_regex() ::
          {literal, char()}
        | {character_range, char(), char()} % Inclusive
        ).
-type(regex() ::
          atomic_regex()
        | {concat, [regex()]}
        | {choice, [regex()]}
        | {repeat, regex()} % One-or-more
        | empty             % Equivalent to {concat, []}
        ).

-type(regex_token() ::
       atomic_regex()
       | begin_group | end_group | choice_separator
       | {repeat, integer(), integer()|infinity}
       ).

%% Precedence levels:
%% - choice
%% - concatenation
%% - repetition
%% - grouping / atomic parts

%% %% Precedences:
%% -define(PREC_LOWEST, 0).
%% -define(PREC_CONCAT, 1).
%% -define(PREC_CHOICE, 2).
%% -define(PREC_REPEAT, 3).
%% -define(PREC_HIGHEST, 100).


%% from_string(RegexStr) ->
%%     case parse_choice(RegexStr, ?PREC_LOWEST) of
%%         {Regex, ""} ->
%%             Regex;
%%         {_Regex, [C|_]=Rest} ->
%%             Pos = length(RegexStr) - length(Rest),
%%             error({regex_parse_error, unexpected_character,
%%                    [{character, C}, {position, Pos}]})
%%     end.

%% %% Grouping.
%% parse("(" ++ Rest, Prec) ->
%%     case parse(Rest, ?PREC_LOWEST) of
%%         {Regex, ")"++Rest2} ->
%%             {Regex, Rest2};
%%         {_Regex, ""} ->
%%             error({regex_parse_error, 'missing_)', []});
%%         {_Regex, _} ->
%%             error({regex_parse_error, 'expected_)', []})
%%     end;
%% parse("\\d"++Rest, Prec) ->

%% parse_choice(RegexStr) -> parse_choice(RegexStr, []);
%% parse_choice(RegexStr, Acc) ->
%%     case parse_concatenation(RegexStr) of


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
    Tokens3 = handle_alternation(Tokens2),
    handle_concatenation_and_repetition(Tokens3).

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
            make_choice(lists:reverse([Choice|Acc]))
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
    Y = handle_repetition(X, N,M),
    handle_concatenation_and_repetition(Rest, [Y | Acc]);
handle_concatenation_and_repetition([X | Rest], Acc) ->
    handle_concatenation_and_repetition(Rest, [X | Acc]).

handle_repetition(X, N,M) ->
    if N==1, M==1 ->
            X;
       N==1, M==infinity ->
            {repeat, X};
       N==0 ->
            make_choice([empty | handle_repetition(X, 1,M)]);
       M<N ->
            error({bad_repetition, N, M});
       N>0 ->
            make_concat([X, handle_repetition(X, N-1, safe_minus_one(M))])
    end.

safe_minus_one(infinity) -> infinity;
safe_minus_one(M) when is_integer(M) -> M-1.


make_concat([X]) -> X;
make_concat(L) -> {concat, L}.

make_choice([X]) -> X;
make_choice(L) -> {choice, L}.

