-module(wsdler_xml).

-export([parse_file/1, parse_string/1, erlsom_options/0]).
-export([attribute/2, attribute/3, list_attribute/2, attribute/4]).

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

attribute(AName, Attrs) ->
    case lists:keyfind({"", AName}, 1, Attrs) of
        {_, Value} -> Value;
        false -> error({no_such_attribute, AName, Attrs})
    end.

attribute(AName, Attrs, Default) ->
    case lists:keyfind({"", AName}, 1, Attrs) of
        {_, Value} -> Value;
        false      -> Default
    end.

attribute(AName, Attrs, ConvertFun, Default) ->
    case lists:keyfind({"", AName}, 1, Attrs) of
        {_, Value} -> ConvertFun(Value);
        false      -> Default
    end.

list_attribute(AName, Attrs) ->
    case lists:keyfind({"", AName}, 1, Attrs) of
        {_, Value} -> string:tokens(Value, " ");
        false -> error({no_such_attribute, AName, Attrs})
    end.


%%%====================
symbolic_name(Name, ?XSD_NS , _) -> {xsd,  Name};
symbolic_name(Name, ?WSDL_NS, _) -> {wsdl, Name};
symbolic_name(Name, ?SOAP_NS, _) -> {soap, Name};
symbolic_name(Name, _ , "xml") -> {xml,  Name}; % Predefined to http://www.w3.org/XML/1998/namespace
symbolic_name(Name, NS,       _) -> {NS, Name}.


qnamePred({attribute, {xsd, "element"},     {[],"ref"}}) -> true;
qnamePred({attribute, {xsd, "element"},     {[],"type"}}) -> true;
qnamePred({attribute, {xsd, "attribute"},   {[],"ref"}}) -> true;
qnamePred({attribute, {xsd, "attribute"},   {[],"type"}}) -> true;
qnamePred({attribute, {xsd, "group"},       {[],"ref"}}) -> true;
qnamePred({attribute, {xsd, "restriction"}, {[],"base"}}) -> true;
qnamePred({attribute, {xsd, "extension"  }, {[],"base"}}) -> true;
qnamePred(_) -> false.
