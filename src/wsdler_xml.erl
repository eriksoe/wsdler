-module(wsdler_xml).

-export([parse_file/1, parse_string/1, erlsom_options/0]).
-export([attribute/2, attribute/3, list_attribute/2, attribute/4]).
-export([unparse/1]).

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

uri_for_namespace(xsd)  -> ?XSD_NS;
uri_for_namespace(wsdl) -> ?WSDL_NS;
uri_for_namespace(soap) -> ?SOAP_NS;
uri_for_namespace(xml)  -> xml;
uri_for_namespace(URI) when is_list(URI) -> URI.

qnamePred({attribute, {xsd, "element"},     {[],"ref"}}) -> true;
qnamePred({attribute, {xsd, "element"},     {[],"type"}}) -> true;
qnamePred({attribute, {xsd, "attribute"},   {[],"ref"}}) -> true;
qnamePred({attribute, {xsd, "attribute"},   {[],"type"}}) -> true;
qnamePred({attribute, {xsd, "group"},       {[],"ref"}}) -> true;
qnamePred({attribute, {xsd, "restriction"}, {[],"base"}}) -> true;
qnamePred({attribute, {xsd, "extension"  }, {[],"base"}}) -> true;
qnamePred(_) -> false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  XML generation  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Unparsing of root node.
unparse({{NS,Tag}, Attrs, Children}=RootNode) ->
    NSTab = ets:new(ns_mapping, [set]),
    try
        true = ets:insert(NSTab, {NS,default_ns}),
        true = ets:insert(NSTab, {xml,"xml"}),
        XML0 = unparse_node2(Tag, Attrs, Children, NSTab),
        NSAttrs = [case NS of
                       default_ns -> {{undefined,"xmlns"},uri_for_namespace(URI)};
                       _          -> {{"xmlns",NS}, uri_for_namespace(URI)}
                   end
                   || {URI,NS} <- ets:tab2list(NSTab), URI /= xml],
        XMLNSAttrs = attrs_to_iolist(NSAttrs, NSTab),
        ["<", Tag, XMLNSAttrs | XML0]
    after
        ets:delete(NSTab)
    end.

unparse_node({Tag, Attrs, Children}, NSTab) ->
    TagQN = qnamify(Tag, NSTab),
    ["<", unparse_tag_qname(TagQN), unparse_node2(TagQN, Attrs, Children, NSTab)].

unparse_node2(Tag, Attrs, Children, NSTab) ->
    [attrs_to_iolist(Attrs, NSTab)
     |
     case Children of
         [] -> "/>";
         _ ->
             [">",
              [unparse_content(C, NSTab) || C <- Children],
              "</", Tag, ">"]
     end].

unparse_content({_,_,_}=Node, NSTab) ->
    unparse_node(Node, NSTab);
unparse_content(Content, _NSTab) when is_list(Content);
                                      is_binary(Content) ->
    %% TODO: Escape this string!
    Content.

attrs_to_iolist(Attrs, NSTab) ->
    [[" ", unparse_attr_qname(qnamify(Attr, NSTab)), "=", "\"", Val, "\""]
     || {Attr, Val} <- Attrs].

unparse_tag_qname({default_ns,Name}) -> Name;
unparse_tag_qname({NSPrefix,Name}) -> [NSPrefix, ":", Name].

unparse_attr_qname({undefined,Name}) -> Name;
unparse_attr_qname({NSPrefix,Name}) -> [NSPrefix, ":", Name].

qnamify({NS,Name}, NSTab) -> {qnamify_ns(NS, NSTab), Name}.

qnamify_ns("", _NSTab) -> undefined;
qnamify_ns(undefined, _NSTab) -> undefined;
qnamify_ns(NS, NSTab) ->
    case uri_for_namespace(NS) of
        xml     -> "xml"; % Predefined.
        "xmlns" -> "xmlns";
        URI ->
            case ets:lookup(NSTab, URI) of
                [{_,Prefix}] ->
                    Prefix;
                [] ->
                    Prefix = "ns"++integer_to_list(ets:info(NSTab,size)),
                    true = ets:insert(NSTab, {URI,Prefix}),
                    Prefix
            end
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

unparse1_test() ->
    XML = lists:flatten(?MODULE:unparse({{"uri","tag"},[],[]})),
    ?assertEqual("<tag xmlns=\"uri\"/>", XML).

unparse2_test() ->
    XML = lists:flatten(?MODULE:unparse({{xsd,"tag"},[],[]})),
    ?assertEqual("<tag xmlns=\""++?XSD_NS++"\"/>", XML).

unparse_children_test() ->
    XML = (catch lists:flatten(?MODULE:unparse({{"uri","outer"},[],[{{"uri","inner"},[],[]}]}))),
    ?assertEqual("<outer xmlns=\"uri\"><inner/></outer>", XML).

unparse_children2_test() ->
    XML = (catch lists:flatten(?MODULE:unparse({{"uri1","outer"},[],[{{"uri2","inner"},[],[]}]}))),
    ?assertEqual("<outer xmlns=\"uri1\" xmlns:ns2=\"uri2\"><ns2:inner/></outer>", XML).

-endif.
