-module(wsdler_xsdfile_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

xsdfiles_test() ->
    TestDataDir = filename:join(filename:join(code:lib_dir(wsdler), "test"), "data"),
    {ok, DataFiles0} = file:list_dir(TestDataDir),
    DataFiles = [filename:join(TestDataDir,X) || X <- DataFiles0, lists:suffix(".xsd",X)],
    [x = (catch one_xsd_file(X)) || X <- DataFiles].

one_xsd_file(Filename) ->
    {ok,XMLFragment} = file:read_file(Filename),
    SchemaXML =
        ["<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\""
        " targetNamespace=\"http://www.example.org\""
        " xmlns=\"http://www.example.org\""
        " elementFormDefault=\"qualified\">",
         XMLFragment,
        "</xsd:schema>"],
    {ok, TypeMap} = wsdler_wsdl:parse_xsd(unicode:characters_to_list(SchemaXML)),
    TypeNames = [TN || {{_,TN}, _} <- dict:to_list(TypeMap)],
    x = TypeNames.



