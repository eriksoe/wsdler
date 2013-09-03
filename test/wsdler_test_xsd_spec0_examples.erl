-module(wsdler_test_xsd_spec0_examples).

-include_lib("eunit/include/eunit.hrl").

-compiler([export_all]).

xsd_spec0_examples_test() ->
    FileName = code:lib_dir(wsdler,test)++"/test-xsd.xml",
    {ok,Text} = file:read_file(FileName),
    Res = case foreach(Text, fun do/1, {0,0}) of
	      {SuccessCounter, 0} ->
		  io:format(user, "Success.  Success runs= ~p~n", [SuccessCounter]),
		  true;
	      {SuccessCounter, ErrorCounter} ->
		  io:format(user, "Failure.  Success runs=~p.  Failure runs=~p~n", [SuccessCounter, ErrorCounter]),
		  false
	  end,
    ?assert(Res).

do(XML) ->
    wsdler_xsd:do_schema(XML).

foreach("\n"++Rest, Fun, Counters) ->
    foreach(Rest, Fun, Counters);
foreach([], Fun, Counters) ->
    Counters;
foreach(Text, Fun, {SuccessCounter, ErrorCounter}) ->
    {ok,XML,Rest} = erlsom:simple_form(Text),
    try
	Fun(XML),
	%% Do some more...
	foreach(Rest, Fun, {SuccessCounter+1, ErrorCounter})
    catch
	Type:Reason ->
	    io:format(user, "~nError running test: type=~p reason=~p.~nStack trace: ~p~n", [Type, Reason, erlang:get_stacktrace()]),
	    foreach(Rest, Fun, {SuccessCounter, ErrorCounter+1})
    end.

