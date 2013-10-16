-module(wsdler_simpletypes_test).

-export([run/0]).

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

prop_string_enum_type() ->
    ?FORALL(Enum, non_empty(list(list(choose($A,$Z)))), % List of easy strings
            begin
                EnumSpec = ["<xsd:enumeration value=\""++S++"\"/>"
                            || S <- Enum],
                ?FORALL(X, simple_type_generator("xsd:string", EnumSpec),
                        lists:member(X, Enum))
            end).

simple_type_generator(BaseType) ->
    simple_type_generator(BaseType, "").
simple_type_generator(BaseType, RestrictionBody) ->
    SchemaSrc = schema(BaseType, RestrictionBody),
    {ok,Schema} = wsdler_xsd:parse_string(SchemaSrc),
    wsdler_generators:generate_type(qtypename("T"), Schema).

schema(BaseType, Body) ->
    "<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\""
        " targetNamespace=\"http://www.example.org\""
        " xmlns=\"http://www.example.org\""
        " elementFormDefault=\"qualified\">"
        "  <xsd:simpleType name=\"T\">"
        "    <xsd:restriction base=\""++BaseType++"\">"
        ++ lists:flatten(Body) ++
        "    </xsd:restriction>"
        "  </xsd:simpleType>"
        "</xsd:schema>".

qtypename(LocalName) ->
     {"http://www.example.org", LocalName}.

property_test_() ->
    {timeout, 60,
     fun() -> ?assert(triq:module(?MODULE)) end}.
