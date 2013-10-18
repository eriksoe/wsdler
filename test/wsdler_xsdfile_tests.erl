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

      "xsd-integer-in-element.xsd", % Defines no types, so not relevant for the generic test

      "test-xsd-45.xsd" % TODO
     ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  XMLLint property tests  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(TGN, "http://www.example.org").
-define(TAG, "tag").

prop_Integer  () -> xmllint("xsd-integer-in-element.xsd",
                            {?TGN,?TAG}).

prop_basicElement() -> xmllint("xsd-integer-in-element.xsd",
                               {?TGN,?TAG}).

prop_Str1     () -> xmllint("simpleType-Str1.xsd", {?TGN, ?TAG}).
prop_Str_3_Ds () -> xmllint("simpleType-Str_3_Ds.xsd", {?TGN, ?TAG}).
prop_StrMinMax() -> xmllint("simpleType-StrMinMax.xsd", {?TGN, ?TAG}).

prop_complexTypeSequence  () -> xmllint("complexType-sequence.xsd", {?TGN,?TAG}).
prop_complexTypeElementish() -> xmllint("complexType-elementish.xsd", {?TGN,?TAG}).

%%% Utils %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xmllint(File, Tag) ->
    ?FORALL(X, gen(Tag, File),
	    xmllintCall(File, X)).

gen(ElemName, Filename) ->
    {ok, Schema} = wsdler_xsd:parse_file(testfile(Filename)),
    %% io:format(user, "DB| sample: ~p\n", [triq_dom:sample(wsdler_generators:generate_element(ElemName, Schema))]),
    wsdler_generators:generate_root_element(ElemName, Schema).

testfile(Name) ->
    filename:join(filename:join(filename:join(code:lib_dir(wsdler), "test"), "data"), Name).

%% TODO, increase performance of the xmllint validation
xmllintCall(Name, Input) ->
    File = testfile(Name),
    Cmd = "echo '"++ Input ++"' | xmllint --schema "++File++" -noout -",
    case os:cmd(Cmd) of
	"- validates"++_ -> true;
	Output -> io:format("xmllint: ~s", [Output]), false
    end.

%% Helper method for interactive shell development
run() ->
    triq:module(?MODULE).

property_test_() ->
    {timeout, 10000,
     fun()-> {ok,_} = (catch {ok,?assert(triq:module(?MODULE))}) end}.
