-module(wsdler_xsd).

%%% Purpose: Conversion of XSD DOMs into internal model.

-export([parse_file/1, parse_string/1, parse_schema_node/1]).
-export([merge_schemas/2]).

-include("wsdler.hrl").
-import(wsdler_xml, [attribute/2, attribute/3, list_attribute/2]).


parse_file(FileName) ->
    {ok,Text} = file:read_file(FileName),
    parse_string(Text).

parse_string(XMLText) ->
    {ok,XMLTree} = wsdler_xml:parse_string(XMLText),
    Types = parse_schema_node(XMLTree),
    {ok, Types}.

merge_schemas(TypeDict1, TypeDict2) ->
    dict:merge(no_conflicts_assumed, TypeDict1, TypeDict2).

%%%%%%%%%%%%% XSD parsing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_schema_node({{xsd,"schema"}, Attrs, Children}) ->
    TgtNS = attribute("targetNamespace", Attrs, ""),
    Types = lists:foldl(fun (X,A)->process_schema_children(X,A,TgtNS) end,
                        [],
                        strip_annotations(Children)),
    build_type_dict(Types).

build_type_dict(Types) when is_list(Types) ->
    lists:foldl(fun({K,V},D) -> dict:store(K,V,D) end,
                dict:new(),
                Types).

%%%%%% <schema> children: %%%%%%%%%%%%%%%%%%%%
%%% (include | import | redefine | annotation)*, ("schemaTop"*, annotation*)
%%% where
%%%   schemaTop ::= ( "redefinable" | element | attribute | notation)
%%%   redefinable ::= (simpleType | complexType | group | attributeGroup)
%%%
process_schema_children({{xsd,"import"}, _Attrs, _Children}, Acc, _TgtNS) ->
%%     io:format("DB| import: ~p\n", [_Attrs]),
    Acc;
process_schema_children(E={{xsd,"element"}, _, _}, Acc, TgtNS) ->
    Element = #element{name=Name} = process_element(E),
    QName = {TgtNS, Name},
    %%io:format("DB| define type: ~s:~s\n  = ~p\n", [TgtNS,TypeName,Type]),
    [{QName, Element#element{name=QName}} | Acc];
process_schema_children(Node={{xsd,"simpleType"}, Attrs,_}, Acc, TgtNS) ->
    TypeName = attribute("name",Attrs),
    %%     io:format("DB| define type: ~s:~s\n  = ~p\n", [TgtNS,TypeName, Type]),
    %%     try
    %%         Gen = wsdler_generators:generator(Type),
    %%         io:format("DB| generator: ~p\n", [Gen]),
    %%         io:format("DB| sample: ~p\n", [triq_dom:sample(Gen)])
    %%     catch _:Reason ->
    %%             io:format("** Generator error: ~p\n", [Reason])
    %%     end,
    [{{TgtNS,TypeName},process_simpleType(Node)} | Acc];
process_schema_children(Node={{xsd,"complexType"}, Attrs, _}, Acc, TgtNS) ->
    TypeName = attribute("name",Attrs),
    %%io:format("DB| define type: ~s:~s\n", [TgtNS,TypeName]),
    [{{TgtNS,TypeName},process_complexType(Node)} | Acc].

process_complexType({{xsd,"complexType"}, _Attrs, Children}) ->
    {Content, Attributes} = process_complexType_children(strip_annotations(Children)),
    #complexType{content = Content, attributes=Attributes}.

%%%%%% <complexType> children: %%%%%%%%%%%%%%%%%%%%
%%% ( annotation?, "complexTypeModel")
%%% where
%%%   complexTypeModel ::= ( simpleContent | complexContent |
%%%                          ( "typeDefParticle"? "attrDecls" ) )
%%%   typeDefParticle ::= ( group | all | choice | sequence )
%%%   attrDecls ::= ( (attribute | attributeGroup)*, anyAttribute? )
%%%
process_complexType_children([]) ->
    {undefined, todo_process_complex_children} ;
process_complexType_children(Attributes=[{{xsd,"attribute"},_,_}|_]) ->
    {undefined, [process_attribute(Attr) || Attr <- Attributes]} ;
process_complexType_children([Node={{xsd,"sequence"},_,_} | Attributes]) ->
    {process_sequence(Node), [process_attribute(Attr) || Attr <- Attributes]} ;
process_complexType_children([Node={{xsd,"choice"},_,_} | Attributes]) ->
    {process_choice(Node), [process_attribute(Attr) || Attr <- Attributes]} ;
process_complexType_children([{{xsd,"simpleContent"},_,[Child]}]) -> %% TODO, only one child for now
    {process_simpleContent_child(Child), []}.

process_sequence({{xsd, "sequence"}, _Attr, Children}) ->
    #sequence{content=[process_sequence_children(Child) || Child <- Children]}.
process_sequence_children(Node={{xsd, "sequence"},_,_}) ->
    process_sequence(Node);
process_sequence_children(Node={{xsd, "element"},_,_}) ->
    process_element(Node);
process_sequence_children(Node={{xsd, "choice"},_,_}) ->
    process_choice(Node).


process_simpleContent_child({{xsd,"extension"},Attributes ,Children}) ->
    BaseTypeQname = attribute("base", Attributes),
    #simpleContentExtension{base = BaseTypeQname,
			    attributes = lists:map(fun(Child) -> process_attribute(Child) end, Children)}.

process_attribute({{xsd, "anyAttribute"},_Attributes, _Children}) ->
    {anyAttribute, todo_anyAttribute};
process_attribute({{xsd, "attribute"},Attributes, Children}) ->
    Name = attribute("name", Attributes),
    Type = attribute("type", Attributes, undefined),
    Use  = attribute("use", Attributes, undefined),
    #attribute{name=Name, type=Type, use=Use,
	       simpleType=case Children of [Child] -> process_simpleType(Child); [] -> undefined end}.

process_element({{xsd,"element"}, Attrs, []}) ->
     case attribute("ref", Attrs, none) of
	none ->
	    ElemName = attribute("name",Attrs),
	    #element{name=ElemName}; % TODO: type
	Qname ->
	    #elementRef{ref=Qname}
    end;
process_element({{xsd,"element"}, Attrs, Children}) ->
    ElemName = attribute("name",Attrs),
    case strip_annotations(Children) of
	[Child] ->
	    #element{name=ElemName, type=process_element_child(Child)};
	[] ->
	    #element{name=ElemName}
end.

process_element_child(Node={{xsd, "simpleType"},_,_}) ->
    process_simpleType(Node);
process_element_child(Node={{xsd, "complexType"},_,_}) ->
    process_complexType(Node).

process_choice({{xsd,"choice"}, _Attr, Children}) ->
    #choice{content=[process_choice_children(Child) || Child <- Children]}.

process_choice_children(Node={{xsd, "sequence"},_,_}) ->
    process_sequence(Node);
process_choice_children(Node={{xsd, "choice"},_,_}) ->
    process_choice(Node);
process_choice_children(Node={{xsd, "element"},_,_}) ->
    process_element(Node).


%%%%%% <simpleType> children: %%%%%%%%%%%%%%%%%%%%
%%% (annotation?, "simpleDerivation")
%%% where
%%%   simpleDerivation ::= (restriction | list | union)
process_simpleType({{xsd, "simpleType"}, _Attr, Children}) ->
    [Child] = strip_annotations(Children),
    process_simpleType_child(Child).

process_simpleType_child({{xsd,"restriction"}, Attrs, Children}) ->
    BaseType = attribute("base", Attrs),
    lists:foldl(fun process_restriction_child/2,
                #restriction{base=BaseType},
                Children);
process_simpleType_child({{xsd,"list"}, Attrs, Children}) ->
    ItemType = case [X || X={{xsd,"simpleType"},_,_} <- Children] of
		   [] ->
		       {named,attribute("itemType", Attrs)};
		   [ItemTypeElement] ->
		       process_simpleType_child(ItemTypeElement) % ?
	       end,
    #simpleListType{itemType=ItemType};
process_simpleType_child({{xsd,"union"}, Attrs, Children}) ->
    MemberTypes = case [X || X={{xsd,"simpleType"},_,_} <- Children] of
		      [] ->
			  [{named,X}
			   || X<-list_attribute("memberTypes", Attrs)];
        MemberTypeElements ->
			  [process_simpleType_child(strip_annotations(X))
                           || X <-MemberTypeElements]
		  end,
    #simpleUnionType{memberTypes=MemberTypes}.

process_restriction_child({{xsd, "enumeration"}, Attrs, _Children}, #restriction{enumeration=EVs}=R) ->
    EnumValue = attribute("value", Attrs),
    R#restriction{enumeration=[EnumValue | EVs]};
process_restriction_child({{xsd, "pattern"}, Attrs, _Children}, #restriction{pattern=undefined}=R) ->
    Pattern = attribute("value", Attrs),
    R#restriction{pattern=Pattern};

process_restriction_child({{xsd, "minLength"}, Attrs, _Children}, #restriction{minLength=undefined}=R) ->
    Length = list_to_integer(attribute("value", Attrs)),
    R#restriction{minLength=Length};
process_restriction_child({{xsd, "maxLength"}, Attrs, _Children}, #restriction{maxLength=undefined}=R) ->
    Length = list_to_integer(attribute("value", Attrs)),
    R#restriction{maxLength=Length};
process_restriction_child({{xsd, "length"}, Attrs, _Children}, #restriction{minLength=undefined, maxLength=undefined}=R) ->
    Length = list_to_integer(attribute("value", Attrs)),
    R#restriction{minLength=Length, maxLength=Length};
process_restriction_child({{xsd, "minExclusive"}, Attrs, _Children}, #restriction{minValue=undefined}=R) ->
    Value = attribute("value", Attrs),
    R#restriction{minValue={Value,false}};
process_restriction_child({{xsd, "minInclusive"}, Attrs, _Children}, #restriction{minValue=undefined}=R) ->
    Value = attribute("value", Attrs),
    R#restriction{minValue={Value,true}};
process_restriction_child({{xsd, "maxExclusive"}, Attrs, _Children}, #restriction{maxValue=undefined}=R) ->
    Value = attribute("value", Attrs),
    R#restriction{maxValue={Value,false}};
process_restriction_child({{xsd, "maxInclusive"}, Attrs, _Children}, #restriction{maxValue=undefined}=R) ->
    Value = attribute("value", Attrs),
    R#restriction{maxValue={Value,true}};
process_restriction_child({{xsd, "fractionDigits"}, Attrs, _Children}, #restriction{}=R) ->
    Value = attribute("value", Attrs),
    R#restriction{fractionDigits=Value};
process_restriction_child({{xsd, "totalDigits"}, Attrs, _Children}, #restriction{}=R) ->
    Value = attribute("value", Attrs),
    R#restriction{totalDigits=Value}.

strip_annotations([{{xsd,"annotation"}, _, _} | Rest]) ->
    strip_annotations(Rest);
strip_annotations(X) ->
    X.

%%%======================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


simpleType_test() ->
    XMLSchema =
	"<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\""
        "     targetNamespace=\"http://www.example.org\""
        "     xmlns=\"http://www.example.org\""
        "     elementFormDefault=\"qualified\">"
	"  <xsd:simpleType name=\"myInteger\">"
	"    <xsd:restriction base=\"xsd:integer\">"
	"      <xsd:minInclusive value=\"10000\"/>"
	"      <xsd:maxInclusive value=\"99999\"/>"
	"     </xsd:restriction>"
	"  </xsd:simpleType>"
        "</xsd:schema>",

    NS = "http://www.example.org",
    Ast =
	[{{NS,"myInteger"},
          #restriction{base={xsd,"integer"},
                       minValue={10000,true},
                       maxValue={99999,true}}}],
    check_test_example(XMLSchema, Ast).

complexType_test() ->
    XMLSchema =
	"<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\""
        "     targetNamespace=\"http://www.example.org\""
        "     xmlns=\"http://www.example.org\""
        "     elementFormDefault=\"qualified\">"
	"  <xsd:complexType name=\"Address\" >"
	"    <xsd:sequence>"
	"      <xsd:element name=\"name\"   type=\"xsd:string\"/>"
	"      <xsd:element name=\"street\" type=\"xsd:string\"/>"
	"      <xsd:element name=\"city\"   type=\"xsd:string\"/>"
	"      <xsd:element name=\"state\"  type=\"xsd:string\"/>"
	"      <xsd:element name=\"zip\"    type=\"xsd:decimal\"/>"
	"    </xsd:sequence>"
	"  </xsd:complexType>"
        "</xsd:schema>",

    Ast =
        [{{"http://www.example.org", "Address"},
          #complexType{children=[
                                 #element{name="name"}, % TODO: types
                                 #element{name="street"},
                                 #element{name="city"},
                                 #element{name="state"},
                                 #element{name="zip"}
                                ]}}],
    check_test_example(XMLSchema, Ast).

check_test_example(XMLSchema, ExpectedTypes) ->
    {ok,TypeDict} = (catch wsdler_xsd:parse_string(XMLSchema)),
    ?assertEqual(ExpectedTypes, lists:sort(dict:to_list(TypeDict))).

-endif.
