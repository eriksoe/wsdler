-module(wsdler_xsd).
-compile(export_all).
-include_lib("triq/include/triq.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsing XML %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type erlsomXML() :: tuple().

%%% Namespaces:
-define(XSD_NS0, "{http://www.w3.org/2001/XMLSchema}").
-define(WSDL_NS0, "{http://schemas.xmlsoap.org/wsdl/}").

-define(XSD_NS, "http://www.w3.org/2001/XMLSchema").
-define(WSDL_NS, "http://schemas.xmlsoap.org/wsdl/").
-define(SOAP_NS, "http://schemas.xmlsoap.org/wsdl/soap/").

parse_xsd_file(FileName) ->
    {ok,Text} = file:read_file(FileName),
    parse_xsd(Text).

-spec parse_xsd(term()) -> [xsdType()].
parse_xsd(XMLText) ->
    {ok,XML,_Rest} = erlsom:simple_form(XMLText, [{nameFun, fun symbolic_name/3}]),
    do_schema(XML).

symbolic_name(Name, ?XSD_NS , _) -> {xsd,  Name};
symbolic_name(Name, ?WSDL_NS, _) -> {wsdl, Name};
symbolic_name(Name, ?SOAP_NS, _) -> {soap, Name};
symbolic_name(Name, NS,       _) -> {NS, Name}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compiling erlsom XML to XSD Ast %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% record definitions must appear first.
-record(restriction, {base,
		      minInclusive = none,
		      maxInclusive = none
		     }).

%%%%% XSD-types Ast %%%%%

-type xsdType()       :: simpleType()
		       | complexType()
		       | primitiveType().
-type primitiveType() :: name().
-type simpleType()    :: {'SIMPLE_TYPE',  typeName(), [#restriction{}]}.
-type complexType()   :: {'COMPLEX_TYPE', typeName(), [element()]}.
-type element()       :: {'ELEMENT', elementName(), xsdType()}.
-type elementName()   :: name().
-type typeName()      :: name().
-type name()          :: atom().

%%% Compiler

-spec do_schema(erlsomXML()) -> [xsdType()].
do_schema({{xsd,"schema"}, Attrs, Types}) ->
    TgtNS = list_to_atom(attribute("targetNamespace", Attrs)),
    lists:map(fun (X) -> do_type(X, TgtNS) end, strip_annotations(Types)).

-spec do_type(erlsomXML(), term()) -> xsdType().
do_type({{xsd,"simpleType"}, Attrs, Restriction}, TgtNS) ->
    TypeName     = list_to_atom(attribute("name",Attrs)),
    Restrictions = lists:map(fun(X) -> do_restriction(X, TgtNS) end,
			     strip_annotations(Restriction)),
    {'SIMPLE_TYPE', {TgtNS,TypeName}, Restrictions};
do_type({{xsd,"complexType"}, _Attrs, [{{xsd, "sequence"}, _Attr2, Elements}]}, TgtNS) ->
    Elems = lists:map(fun(X) -> do_element(X, TgtNS) end,
		      strip_annotations(Elements)),
    {'COMPLEX_TYPE', name, Elems}.

-spec do_element(erlsomXML(), term()) -> element().
do_element({{xsd,"element"}, Attrs, []}, _TgtNS) ->
    Name     = list_to_atom(attribute("name",Attrs)),
    TypeName = list_to_atom(attribute("type", Attrs)),
    {'ELEMENT', Name, TypeName}.

-spec do_restriction(erlsomXML(), term()) -> #restriction{}.
do_restriction({{xsd,"restriction"}, Attrs, Restrictions}, TgtNS) ->
    BaseType = list_to_atom(attribute("base", Attrs)),
    lists:foldl(fun(X, Restriction) -> do_restriction_field(X, Restriction, TgtNS) end,
		#restriction{base=BaseType},
		strip_annotations(Restrictions)).

do_restriction_field({{xsd, "minInclusive"}, Attrs, []}, Restriction, _TgtNS) ->
    Length = list_to_integer(attribute("value", Attrs)),
    Restriction#restriction{minInclusive=Length};
do_restriction_field({{xsd, "maxInclusive"}, Attrs, []}, Restriction, _TgtNS) ->
    Length = list_to_integer(attribute("value", Attrs)),
    Restriction#restriction{maxInclusive=Length}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

strip_annotations([{{xsd,"annotation"}, _, _} | Rest]) ->
    strip_annotations(Rest);
strip_annotations(X) ->
    X.

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test() ->
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

    NS = 'http://www.example.org',
    Ast =
	[{'SIMPLE_TYPE',
	  {NS,myInteger},
	  #restriction{base='xsd:integer', minInclusive=10000, maxInclusive=99999}},
	 {'COMPLEX_TYPE', name, [{'ELEMENT', name,   'xsd:string'},
				 {'ELEMENT', street, 'xsd:string'},
				 {'ELEMENT', city,   'xsd:string'},
				 {'ELEMENT', state,  'xsd:string'},
				 {'ELEMENT', zip,    'xsd:decimal'}
				]}],
    do_test(XMLSchema, Ast).

do_test(XMLSchema, AstExpected) ->
    {ok,XML,_Rest} = erlsom:simple_form(XMLSchema, [{nameFun, fun symbolic_name/3}]),
    Ast = do_schema(XML),
    case AstExpected of
	Ast ->
	    ok;
	_ ->
	    io:format("*** XMLTree ***~n~p~n~n", [XML]),
	    io:format("*** Excepted AST ***~n~p~n~n", [AstExpected]),
	    io:format("*** Actual AST ***~n~p~n~n", [Ast]),
	    error
    end.


%%% Some experiments.....

gen_type ({'SIMPLE_TYPE', Name, #restriction{base='xsd:integer', minInclusive=Min, maxInclusive=Max}})
  when is_integer(Min), is_integer(Max)->
    ?LET(X, int(Min, Max),
	 io_lib:format("<~p>~p<~p\\>~n", [Name, X, Name])).

gen_test() ->
    io:format(triq_dom:sample(
		gen_type({'SIMPLE_TYPE', uha, #restriction{base='xsd:integer', minInclusive=3, maxInclusive=9}}))).
