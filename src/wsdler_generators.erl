-module(wsdler_generators).

-export([generate_root_element/2]).
-export([generate_element/2, generate_type/2]).

-include("wsdler.hrl").
-include_lib("triq/include/triq.hrl").

generate_root_element(ElemName, Schema) ->
    ?LET(Tree,
         generate_element(ElemName, Schema),
         lists:flatten(
           wsdler_xml:unparse(Tree))).

generate_element(ElemRef, Schema) ->
    generate_element(ElemRef, Schema, []).
generate_element(ElemRef, Schema, Attrs) ->
    case wsdler_xsd:lookup_element(ElemRef, Schema) of
	#element{name=Name, type=Type} ->
            Contents = case generate_type0(Type, Schema) of
                           {simple, Contents0} -> [{text,Contents0}];
                           {complex, Contents0} -> Contents0
                       end,
            %% TODO: Handle Attrs!
            {return(Name), [], Contents}
	    %% ?LET(BodyGen, generate_type(Type, Schema),
	    %%      lists:flatten(
            %%        wsdler_xml:unparse({Name,Attrs,BodyGen})))
		   %% xml_to_iolist(
		   %%   xml(Name, Attrs, BodyGen))))
    end.

generate_type(Type, Schema) ->
    {_,Value} = generate_type0(Type,Schema),
    Value.

generate_type0({xsd, Prim}, _Schema) ->
    {simple, generate_xsd_type(Prim)};
generate_type0(Type, Schema) ->
    case wsdler_xsd:lookup_type(Type, Schema) of
        #complexType{content=T} ->
	    {complex, generate_complexType(T, Schema)};
        #simpleType{type=TypeDef} ->
            {simple,  generate_simpleType(TypeDef)}
    end.

%% TODO, to simplistic! Improve!
generate_xsd_type("string") ->
    string_gen(1, undefined);
generate_xsd_type("boolean") ->
    oneof(["false","true","0","1"]);
generate_xsd_type("integer") ->
    ?LET(I, int(),
	 integer_to_list(I)).

generate_complexType(F=#simpleContentRestriction{}, _Schema) -> {todo, F};
generate_complexType(F=#simpleContentExtension{}, _Schema) -> {todo, F};
generate_complexType(F=#complexContentExtension{}, _Schema) -> {todo, F};
generate_complexType(#complexContentRestriction{base={xsd, "anyType"}, children=Element_ish, attributes=[]}, Schema) ->
    generate_element_ish(Element_ish, Schema).

generate_element_ish(#sequence{content=Elems}, Schema) ->
    ?LETSHRINK(Groups,[generate_element_ish(Elem, Schema) || Elem <- Elems],
               lists:concat(Groups));
generate_element_ish(#choice{content=Choices}, Schema) ->
    generate_element_ish(lists:nth(random:uniform(length(Choices)),
				   Choices),
			 Schema);
generate_element_ish(#element_instantiation{element_ref=ElemRef, minOccurs=Min, maxOccurs=Max}, Schema) ->
    Optional = case Max of unbounded -> 10; _ -> Max - Min end,
    ?DELAY([generate_element(ElemRef, Schema) || _N <- lists:seq(1, Min+random:uniform(Optional))]).

generate_simpleType(#restriction{enumeration=Enum}) when Enum /= [] ->
    oneof([return(X) || X <- Enum]);
generate_simpleType(#restriction{base={xsd,"boolean"}}) ->
    oneof(["false","true","0","1"]);
generate_simpleType(#restriction{base="dateTime"}) ->
    %% TODO: Obey facets: min/max-Inclusive/Exclusive, and pattern
    ?LET(X, resize(5,real()),
         format_years_from_now_as_datetime(math:pow(X,7)));
generate_simpleType(#restriction{base={xsd, "string"}, pattern=Pattern}) when Pattern /= undefined ->
    %% TODO: Obey facets: min/max-Inclusive/Exclusive, and minLength/maxLength
    Regex = wsdler_regex:from_string(Pattern),
    wsdler_regex:to_generator(Regex);
generate_simpleType(#restriction{base={xsd,"string"}, minLength=MinLen, maxLength=MaxLen}) ->
    string_gen(MinLen, MaxLen);
generate_simpleType(#simpleUnionType{memberTypes=Types}) ->
    oneof([generate_simpleType(T) || T <- Types]);
generate_simpleType(#simpleListType{itemType=ItemType}) ->
    ?LET(Xs, list(generate_simpleType(ItemType)),
         string:join(Xs, " ")).

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
       MinLen==MaxLen ->
	    [char_gen() || _ <- lists:seq(1,MinLen)];
       true ->
	    ?LET(X, choose(MinLen,MaxLen),
                 [char_gen() || _ <- lists:seq(1,X)])
    end.

char_gen() ->
    frequency([{20,char()},
	       %% TODO, xmllint complains for the lines below:
	       %%   -:1: parser error : Input is not proper UTF-8, indicate encoding !
               {5,choose(128,255)},
               {2,choose(256,16#D7FF)},
               {1,choose(16#E000,16#FFFD)}]).

