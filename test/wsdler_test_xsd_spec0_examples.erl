-module(wsdler_test_xsd_spec0_examples).

-include_lib("eunit/include/eunit.hrl").

-compiler([export_all]).

xsd_spec0_examples_test_() ->
    FileName = code:lib_dir(wsdler,test)++"/test-xsd.xml",
    {ok,Text} = file:read_file(FileName),
    foreach(Text, fun gen_test/1, []).

gen_test(XML) ->
    fun() -> ?assertMatch([_], wsdler_xsd:do_schema(XML)) end.

foreach("\n"++Rest, Fun, Acc) ->
    foreach(Rest, Fun, Acc);
foreach([], _Fun, Acc) ->
    lists:reverse(Acc);
foreach(Text, Fun, Acc) ->
    {ok,XML,Rest} = erlsom:simple_form(Text, wsdler_xml:erlsom_options()),
    foreach(Rest, Fun, [Fun(XML)|Acc]).
