-module(wsdler_simpletypes_test).

-include("../src/wsdler.hrl").

-include_lib("eunit/include/eunit.hrl").
-undef(LET).
-include_lib("triq/include/triq.hrl").

prop_boolean_type() ->
    ?FORALL(X, simple_type_generator("xsd:boolean"),
            lists:member(X, ["0","1","false","true"])).

prop_string_type() ->
    ?FORALL(X, simple_type_generator("xsd:string"),
            lists:all(fun(C)->is_integer(C) andalso C>=0 andalso C<16#FFFE end,
                     X)).

simple_type_generator(BaseType) ->
    SchemaSrc = schema(BaseType),
    {ok,TypeDict} = wsdler_wsdl:parse_xsd(SchemaSrc),
    Type = dict:fetch(qtypename("T"), TypeDict),
    wsdler_generators:generator(Type).

schema(BaseType) ->
    "<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\""
        " targetNamespace=\"http://www.example.org\""
        " xmlns=\"http://www.example.org\""
        " elementFormDefault=\"qualified\">"
        "<xsd:simpleType name=\"T\"><xsd:restriction base=\""++BaseType++"\"/></xsd:simpleType>"
        "</xsd:schema>".

qtypename(LocalName) ->
     {"http://www.example.org", LocalName}.

property_test_() ->
    {timeout, 60,
     fun() -> ?assert(triq:module(?MODULE)) end}.
