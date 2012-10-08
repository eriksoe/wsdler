-module(wsdler_wsdl).
%%% Parsing of WSDL files.

-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

%%% Namespaces:
-define(XSD_NS0, "{http://www.w3.org/2001/XMLSchema}").
-define(WSDL_NS0, "{http://schemas.xmlsoap.org/wsdl/}").

-define(XSD_NS, "http://www.w3.org/2001/XMLSchema").
-define(WSDL_NS, "http://schemas.xmlsoap.org/wsdl/").
-define(SOAP_NS, "http://schemas.xmlsoap.org/wsdl/soap/").

%-type(typedef(), #simpleType{} | #complexType{}).

-record(definitions, {
          types=[]
         }).
-record(types, {
          types=[]
         }).

%% Simpletype choices:
-record(simpleRestriction,
        {base,
         patterns=[],
         enumeration=[],
         minLength=undefined,
         maxLength=undefined,
         minValue=undefined, % {Value,Inclusive}
         maxValue=undefined, % {Value,Inclusive}
         fractionDigits=undefined,
         totalDigits=undefined
        }).
-record(simpleListType,  {itemType}).
-record(simpleUnionType, {memberTypes}).
-type(simpleDerivation() :: #simpleRestriction{} | #simpleListType{} | #simpleUnionType{}).

-record(simpleType, {type :: {named,_} | simpleDerivation()}).
-record(complexType, {}).
-record(element, {name :: _, type :: #simpleType{}}).

main(File) ->
    dom_parse(File),
    ok.


%%%========== Using "Simple DOM with nameFun" model of erlsom ==========

dom_parse(File) ->
    {ok, WSDL, _} = erlsom:simple_form_file(File, [{nameFun, fun symbolic_name/3}]),
    R = process_wsdl(WSDL),
    io:format("R = ~p\n", [R]),
    ok.

symbolic_name(Name, ?XSD_NS , _) -> {xsd,  Name};
symbolic_name(Name, ?WSDL_NS, _) -> {wsdl, Name};
symbolic_name(Name, ?SOAP_NS, _) -> {soap, Name};
symbolic_name(Name, NS,       _) -> {NS, Name}.


process_wsdl({{wsdl, "definitions"}, _Attrs, Children}) ->
    lists:foldl(fun process_defs_children/2, #definitions{}, Children).

process_defs_children({{wsdl, "types"}, _Attrs, Children}, #definitions{}=Acc) ->
    Types = lists:foldl(fun process_types_children/2, [], Children),
    Acc#definitions{types = Types++Acc#definitions.types};
process_defs_children({{wsdl, "message"}, _Attrs, _Children}, Acc) ->
    [{message,_Attrs}|Acc];
process_defs_children({{wsdl, "portType"}, _Attrs, _Children}, Acc) ->
    [portType|Acc];
process_defs_children({{wsdl, "binding"}, _Attrs, _Children}, Acc) ->
    [binding|Acc];
process_defs_children({{wsdl, "service"}, _Attrs, _Children}, Acc) ->
    [service|Acc].

process_types_children({{xsd,"schema"}, Attrs, Children}, Acc) ->
    TgtNS = attribute("targetNamespace", Attrs),
    Types = lists:foldl(fun (X,A)->process_schema_children(X,A,TgtNS) end,
                        Acc,
                        Children),
    Types.

process_schema_children({{xsd,"import"}, _Attrs, _Children}, Acc, _TgtNS) ->
    io:format("DB| import: ~p\n", [_Attrs]),
    Acc;
process_schema_children({{xsd,"element"}, Attrs, Children}, Acc, TgtNS) ->
    TypeName = attribute("name",Attrs),
    TypeContent =
        case strip_annotations(Children) of
            [{{xsd,"complexType"},_,_}] -> {complexType, 'TODO'};
            [{{xsd,"simpleType"},_,_}] -> {simpleType, 'TODO'};
            [] -> {simpleType, {name,attribute("type",Attrs)}}
        end,
    Type = #element{name={TgtNS, TypeName}, type=TypeContent},
    io:format("DB| define type: ~s:~s\n  = ~p\n", [TgtNS,TypeName,Type]),
    [{{TgtNS,TypeName},Type} | Acc];
process_schema_children({{xsd,"simpleType"}, Attrs, Children}, Acc, TgtNS) ->
    TypeName = attribute("name",Attrs),
    Type = process_simpleType_children(Children),
    io:format("DB| define type: ~s:~s\n  = ~p\n", [TgtNS,TypeName, Type]),
    [{{TgtNS,TypeName},Type} | Acc];
process_schema_children({{xsd,"complexType"}, Attrs, _Children}, Acc, TgtNS) ->
    TypeName = attribute("name",Attrs),
    io:format("DB| define type: ~s:~s\n", [TgtNS,TypeName]),
    [{{TgtNS,TypeName},dummy} | Acc].


process_simpleType_children(L) ->
    [SimpleTypeChildElement] = strip_annotations(L),
    process_simpleType(SimpleTypeChildElement).

process_simpleType({{xsd,"restriction"}, Attrs, Children}) ->
    BaseType = attribute("base", Attrs),
    lists:foldl(fun process_restriction_children/2,
                #simpleRestriction{base=BaseType},
                Children);
process_simpleType({{xsd,"list"}, Attrs, Children}) ->
    case [X || X={{xsd,"simpleType"},_,_} <- Children] of
        [] ->
            ItemType = {named,attribute("itemType", Attrs)};
        [ItemTypeElement] ->
            ItemType = process_simpleType_children(ItemTypeElement)
    end,
    #simpleListType{itemType=ItemType};
process_simpleType({{xsd,"union"}, Attrs, Children}) ->
    case [X || X={{xsd,"simpleType"},_,_} <- Children] of
        [] ->
            MemberTypes = [{named,X}
                           || X<-list_attribute("memberTypes", Attrs)];
        MemberTypeElements ->
            MemberTypes = [process_simpleType_children(strip_annotations(X))
                           || X <-MemberTypeElements]
    end,
    #simpleUnionType{memberTypes=MemberTypes}.

process_restriction_children({{xsd, "enumeration"}, Attrs, _Children}, #simpleRestriction{enumeration=EVs}=R) ->
    EnumValue = attribute("value", Attrs),
    R#simpleRestriction{enumeration=[EnumValue | EVs]};
process_restriction_children({{xsd, "pattern"}, Attrs, _Children}, #simpleRestriction{patterns=Ps}=R) ->
    Pattern = attribute("value", Attrs),
    R#simpleRestriction{patterns=[Pattern | Ps]};
process_restriction_children({{xsd, "minLength"}, Attrs, _Children}, #simpleRestriction{minLength=undefined}=R) ->
    Length = list_to_integer(attribute("value", Attrs)),
    R#simpleRestriction{minLength=Length};
process_restriction_children({{xsd, "maxLength"}, Attrs, _Children}, #simpleRestriction{maxLength=undefined}=R) ->
    Length = list_to_integer(attribute("value", Attrs)),
    R#simpleRestriction{maxLength=Length};
process_restriction_children({{xsd, "length"}, Attrs, _Children}, #simpleRestriction{minLength=undefined, maxLength=undefined}=R) ->
    Length = list_to_integer(attribute("value", Attrs)),
    R#simpleRestriction{minLength=Length, maxLength=Length};
process_restriction_children({{xsd, "minExclusive"}, Attrs, _Children}, #simpleRestriction{minValue=undefined}=R) ->
    Value = attribute("value", Attrs),
    R#simpleRestriction{minValue={Value,false}};
process_restriction_children({{xsd, "minInclusive"}, Attrs, _Children}, #simpleRestriction{minValue=undefined}=R) ->
    Value = attribute("value", Attrs),
    R#simpleRestriction{minValue={Value,true}};
process_restriction_children({{xsd, "maxExclusive"}, Attrs, _Children}, #simpleRestriction{maxValue=undefined}=R) ->
    Value = attribute("value", Attrs),
    R#simpleRestriction{maxValue={Value,false}};
process_restriction_children({{xsd, "maxInclusive"}, Attrs, _Children}, #simpleRestriction{maxValue=undefined}=R) ->
    Value = attribute("value", Attrs),
    R#simpleRestriction{maxValue={Value,true}};
process_restriction_children({{xsd, "fractionDigits"}, Attrs, _Children}, #simpleRestriction{}=R) ->
    Value = attribute("value", Attrs),
    R#simpleRestriction{fractionDigits=Value};
process_restriction_children({{xsd, "totalDigits"}, Attrs, _Children}, #simpleRestriction{}=R) ->
    Value = attribute("value", Attrs),
    R#simpleRestriction{totalDigits=Value}.

strip_annotations([{{xsd,"annotation"}, _, _} | Rest]) ->
    strip_annotations(Rest);
strip_annotations(X) ->
    X.

%%%====================

attribute(AName, Attrs) ->
    case lists:keyfind({"", AName}, 1, Attrs) of
        {_, Value} -> Value;
        false -> error({no_such_attribute, AName, Attrs})
    end.

list_attribute(AName, Attrs) ->
    case lists:keyfind({"", AName}, 1, Attrs) of
        {_, Value} -> string:tokens(Value, " ");
        false -> error({no_such_attribute, AName, Attrs})
    end.
