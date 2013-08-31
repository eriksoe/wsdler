-module(wsdler_generators).

-compile(export_all).

-include("wsdler.hrl").
-include_lib("triq/include/triq.hrl").

generator(#element{name=ElmName, type=Type}, WSDL) ->
    make_element(ElmName, [], generator(Type, WSDL));
generator(#simpleType{type={named,TypeName}}=ST,
          #wsdl{typedict=TypeDict}=WSDL) ->
    io:format("DB| generator(#simpleType): ST=~p\n"),
    TypeDef = dict:fetch(TypeName, TypeDict),
    generator(TypeDef,WSDL);
generator(#simpleType{type=Type},WSDL) ->
    generator(Type,WSDL);
generator(Type,_) ->
    generator(Type).

make_element({NS,Name}, Attrs, Content) ->
    {Name, [{'xmlns',NS} | Attrs], Content}.

generator(#simpleRestriction{enumeration=Enum}) when Enum /= [] ->
    oneof([return(X) || X <- Enum]);
generator(#simpleRestriction{base={xsd,"boolean"}}) ->
    oneof(["false","true","0","1"]);
generator(#simpleRestriction{base="dateTime"}) ->
    %% TODO: Obey facets: min/max-Inclusive/Exclusive, and pattern
    ?LET(X, resize(5.0,real()),
         format_years_from_now_as_datetime(math:pow(X,7)));
generator(#simpleRestriction{base="string", pattern=Pattern}) when Pattern /= undefined ->
    %% TODO: Obey facets: min/max-Inclusive/Exclusive, and minLength/maxLength
    Regex = wsdler_regex:from_string(Pattern),
    wsdler_regex:to_generator(Regex);
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
generator(#simpleUnionType{memberTypes=Types}) ->
    oneof([generator(T) || T <- Types]);
generator(_Other) ->
    {'TODO',_Other}.

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
%%% (Much heavy lifting done by the wsdler_regex module.)
