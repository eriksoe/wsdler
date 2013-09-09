-module(wsdler_test_xsd_spec0_examples).

-include_lib("eunit/include/eunit.hrl").

-compiler([export_all]).

xsd_spec0_examples_test_() ->
    FileName = filename:join(code:lib_dir(wsdler,test), "test-xsd.xml"),
    {ok,Text} = file:read_file(FileName),
    Schemas = split_schemas(Text),
    {foreach, fun()->ok end,
     [{lists:flatten(io_lib:format("Schema #~p", [Nr])), % title
       test_schema(Schema)}
      || {Nr, Schema} <- Schemas]}.
    %% foreach(Text, fun gen_test/1, []).

test_schema(XML) ->
    fun() ->
            TypeMap = wsdler_xsd:schema_to_type_list(wsdler_xsd:parse_schema_node(XML)),
            try ?assertMatch([_|_], TypeMap)
            catch
                Cls:Err ->
                    Trace = erlang:get_stacktrace(),
                    io:format(user, "Exception: ~s:~p\n  Trace: ~p\n",
                              [Cls,Err,Trace]),
                    erlang:raise(Cls,Err,Trace)
            end
    end.

split_schemas(Text) ->
    split_schemas(Text, 0, []).
split_schemas("\n"++Rest, Nr, Acc) ->
    split_schemas(Rest, Nr, Acc);
split_schemas([], _Nr, Acc) ->
    lists:reverse(Acc);
split_schemas(Text, Nr, Acc) ->
    {ok,XML,Rest} = erlsom:simple_form(Text, wsdler_xml:erlsom_options()),
    split_schemas(Rest, Nr+1, [{Nr+1,XML} | Acc]).
