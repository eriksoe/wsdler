-module(wsdler_bad_xsds_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

bad_xsds_test_() ->
    TestDataDir = filename:join(filename:join(code:lib_dir(wsdler), "test"), filename:join("data","bad")),
    {ok, DataFiles0} = file:list_dir(TestDataDir),
    DataFiles = [filename:join(TestDataDir,X)
                 || X <- lists:sort(DataFiles0),
                    lists:suffix(".xsd",X)],
    [{"Schema file "++X, fun()->{ok,_}=(catch {ok,one_xsd_file(X)}) end}
     || X <- DataFiles].

one_xsd_file(Filename) ->
    {ok, Text} = file:read_file(Filename),
    {match, [ExpectedText]} = re:run(Text, "^\\s*Expected:(.*)$", [multiline,{capture,all_but_first,list}]),
    {ok,ExpectedError} = erl_parse(ExpectedText),
    try wsdler_xsd:parse_file(Filename) of
        {ok, Schema} ->
            error({unexpected_success, Schema})
    catch _:Error ->
            ?assertEqual(ExpectedError, Error)
    end.

erl_parse(S) ->
    {ok, Tokens, _Line} = erl_scan:string(S),
    {ok, _Term} = erl_parse:parse_term(Tokens++[{dot,-1}]).
