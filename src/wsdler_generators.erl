-module(wsdler_generators).

-compile(export_all).

-include("wsdler.hrl").
-include_lib("triq/include/triq.hrl").

generator(#element{name=ElmName, type=Type}, WSDL) ->
    make_element(ElmName, [], generator(Type, WSDL));
generator(#simpleType{type={named,TypeName}},
          #wsdl{typedict=TypeDict}=WSDL) ->
    TypeDef = dict:fetch(TypeName, TypeDict),
    generator(TypeDef,WSDL);
generator(#simpleType{type=Type},WSDL) ->
    generator(Type,WSDL);
generator(Type,_) ->
    generator(Type).

make_element({NS,Name}, Attrs, Content) ->
    {Name, [{'xmlns',NS} | Attrs], Content}.

generator(#restriction{enumeration=Enum}) when Enum /= [] ->
    oneof([return(X) || X <- Enum]);
generator(#restriction{base={xsd,"boolean"}}) ->
    oneof(["false","true","0","1"]);
generator(#restriction{base="dateTime"}) ->
    %% TODO: Obey facets: min/max-Inclusive/Exclusive, and pattern
    ?LET(X, resize(5,real()),
         format_years_from_now_as_datetime(math:pow(X,7)));
generator(#restriction{base="string", pattern=Pattern}) when Pattern /= undefined ->
    %% TODO: Obey facets: min/max-Inclusive/Exclusive, and minLength/maxLength
    Regex = wsdler_regex:from_string(Pattern),
    wsdler_regex:to_generator(Regex);
generator(#restriction{base={xsd,"string"}, minLength=MinLen, maxLength=MaxLen}) ->
    string_gen(MinLen, MaxLen);
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

string_gen(MinLen0, MaxLen) ->
    MinLen = if MinLen0==undefined -> 0;
                true -> MinLen0
             end,
    if MaxLen==undefined ->
            ?LET(X, int(),
                 [char_gen() || _ <- lists:seq(1,MinLen+abs(X))]);
       true ->
            ?LET(X, choose(MinLen,MaxLen),
                 [char_gen() || _ <- lists:seq(1,X)])
    end.

char_gen() ->
    frequency([{20,char()},
               {5,choose(128,255)},
               {2,choose(256,16#D7FF)},
               {1,choose(16#E000,16#FFFD)}]).
