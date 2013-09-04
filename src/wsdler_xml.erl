-module(wsdler_xml).

-export([parse_file/1, parse_string/1, erlsom_options/0]).

%%% Purpose: XML handling - parsing and unparsing of XML.
%%%
%%% Parsing/unparsing is done using "simple DOM with nameFun" mode of erlsom.

%%% Namespaces:
-define(XSD_NS, "http://www.w3.org/2001/XMLSchema").
-define(WSDL_NS, "http://schemas.xmlsoap.org/wsdl/").
-define(SOAP_NS, "http://schemas.xmlsoap.org/wsdl/soap/").

parse_file(Filename) ->
    handle_parsing_result(erlsom:simple_form_file(Filename, erlsom_options())).

parse_string(String) ->
    handle_parsing_result(erlsom:simple_form(String, erlsom_options())).

erlsom_options() ->
    [{nameFun, fun symbolic_name/3},
     {qnamePredicate, fun qnamePred/1}].

handle_parsing_result({ok, XMLTree, _Remainder}) ->
    %% TODO: Verify that remainder is only whitespace.
    {ok, XMLTree};
handle_parsing_result({error, _}=Err) ->
    Err.

%%%====================
symbolic_name(Name, ?XSD_NS , _) -> {xsd,  Name};
symbolic_name(Name, ?WSDL_NS, _) -> {wsdl, Name};
symbolic_name(Name, ?SOAP_NS, _) -> {soap, Name};
symbolic_name(Name, NS,       _) -> {NS, Name}.

qnamePred({attribute, {xsd, "restriction"}, {_,"base"}}) -> true;
qnamePred(_) -> false.
