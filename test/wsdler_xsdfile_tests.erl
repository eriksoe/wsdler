-module(wsdler_xsdfile_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-undef(LET).
-include_lib("triq/include/triq.hrl").

xsdfiles_test_() ->
    TestDataDir = filename:join(filename:join(code:lib_dir(wsdler), "test"), "data"),
    {ok, DataFiles0} = file:list_dir(TestDataDir),
    DataFiles = [filename:join(TestDataDir,X)
                 || X <- lists:sort(DataFiles0),
                    lists:suffix(".xsd",X),
                    not lists:member(X, blacklist())],
    [{"Schema file "++X, fun()->{ok,_}=(catch {ok,one_xsd_file(X)}) end}
     || X <- DataFiles].

one_xsd_file(Filename) ->
    {ok, Schema} = wsdler_xsd:parse_file(Filename),
    TypeNames = [TN || {{_,TN}, _} <- wsdler_xsd:schema_to_type_list(Schema)],
    ?assertMatch([_|_], TypeNames),
    %% TODO: Generate samples for each type...
    TypeNames.


blacklist() ->
    %% These are test XSD files which are at present not valid:
    [ %% Invalid as verified with xmllint:
     "test-xsd-17.xsd",
     "test-xsd-18.xsd",
     "test-xsd-19.xsd",
     "test-xsd-20.xsd",
     "test-xsd-40.xsd",
     "test-xsd-41.xsd",
      %% Incomplete:
      "test-xsd-1.xsd", % Missing Items
      "test-xsd-16.xsd", % Missing USAddress, Items
      "test-xsd-26.xsd", % Missing ipo:Address
      "test-xsd-27.xsd", % Missing UKPostcode
      "test-xsd-28.xsd", % Missing UKPostcode
      "test-xsd-29.xsd", % Missing Address, PurchaseOrderType
      "test-xsd-30.xsd", % Using redefine
      "test-xsd-31.xsd", % Missing string
      "test-xsd-35.xsd", % Missing Postcode
      "test-xsd-36.xsd", % Missing Postcode
      "test-xsd-38.xsd", % Importing type "SKU"
      "test-xsd-39.xsd", % Missing Items
      "test-xsd-42.xsd", % Missing USAddress

      "xsd-integer-in-element.xsd" % No support for element with xsd base types yet...
     ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  XMLLint property tests  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TODO, make this test work.  Note the black-list
%%prop_Integer  () -> xmllint("xsd-integer-in-element.xsd").

prop_Str1     () -> xmllint("simpleType-Str1.xsd").
prop_Str_3_Ds () -> xmllint("simpleType-Str_3_Ds.xsd").
prop_StrMinMax() -> xmllint("simpleType-StrMinMax.xsd").

%%% Utils %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(TAG, "tag").
-define(TGN, "http://www.example.org").

xmllint(File) ->
    ?FORALL(X, gen(?TAG, File),
	    xmllintCall(File, X)).

gen(Type, Filename) ->
    {ok, Schema} = wsdler_xsd:parse_file(testfile(Filename)),
    wsdler_generators:generate({?TGN, Type}, Schema).

testfile(Name) ->
    filename:join(filename:join(filename:join(code:lib_dir(wsdler), "test"), "data"), Name).

%% TODO, increase performance of the xmllint validation
xmllintCall(Name, Input) ->
    File = testfile(Name),
    Cmd = "echo '"++ Input ++"' | xmllint --schema "++File++" -noout -",
    case os:cmd(Cmd) of
	"- validates"++_ -> true;
	_ -> false
    end.

%% Helper method for interactive shell development
run() ->
    triq:module(?MODULE).
