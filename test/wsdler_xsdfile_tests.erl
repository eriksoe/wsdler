-module(wsdler_xsdfile_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

xsdfiles_test_() ->
    TestDataDir = filename:join(filename:join(code:lib_dir(wsdler), "test"), "data"),
    {ok, DataFiles0} = file:list_dir(TestDataDir),
    DataFiles = [filename:join(TestDataDir,X) || X <- DataFiles0, lists:suffix(".xsd",X)],
    [{"Schema file "++X, fun()->{ok,_}=(catch {ok,one_xsd_file(X)}) end} || X <- DataFiles].

one_xsd_file(Filename) ->
    {ok, Schema} = wsdler_xsd:parse_file(Filename),
    TypeNames = [TN || {{_,TN}, _} <- wsdler_xsd:schema_to_type_list(Schema)],
    %% TODO: Generate samples for each type...
    TypeNames.



