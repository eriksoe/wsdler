-module(wsdler_wsdl).

%%% Purpose: Conversion of WSDL DOMs into internal model.

-compile(export_all).

-include("wsdler.hrl").

%%%==================== Drivers: ========================================

main(Filename) ->
    WSDL = parse_file(Filename),
    io:format("WSDL = ~p\n", [WSDL]),
    Schema = WSDL#wsdl.typedict,
    TypeList = wsdler_xsd:schema_to_type_list(Schema),
    TypeNames = [TN || {TN,_} <- TypeList],
    io:format("Types = ~p\n", [TypeNames]),
    lists:foreach(fun(TN) ->
                          Gen = wsdler_generators:generator(TN, Schema),
                          io:format("Type sample for ~p:\n  ~p\n",
                                    [TN, triq_dom:sample(Gen)])
                  end,
                  TypeNames),
    WSDL.

parse_file(Filename) ->
    {ok, XMLTree} = wsdler_xml:parse_file(Filename),
    process_wsdl(XMLTree).

%%%%%%%%%%%%% WSDL parsing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec(process_wsdl/1 :: (erlsom_dom()) -> #definitions{}).
process_wsdl({{wsdl, "definitions"}, _Attrs, Children}) ->
    close_definitions(lists:foldl(fun process_defs_children/2,
                                  #definitions{types=wsdler_xsd:empty_schema()}, Children)).

close_definitions(#definitions{types=TypeDict}) ->
    #wsdl{typedict=TypeDict}.

add_to_field(Record,Key,Value) ->
    setelement(Key,Record, [Value | element(Key,Record)]).

process_defs_children({{wsdl, "types"}, _Attrs, Children}, #definitions{}=Acc) ->
    Types = lists:foldl(fun process_types_children/2, wsdler_xsd:empty_schema(), Children),
    Acc#definitions{types = wsdler_xsd:merge_schemas(Types,Acc#definitions.types)};
process_defs_children({{wsdl, "message"}, Attrs, _Children}, Acc) ->
    add_to_field(Acc, #definitions.messages, {message,Attrs});
%%     [{message,_Attrs}|Acc];
process_defs_children({{wsdl, "portType"}, _Attrs, _Children}, Acc) ->
%%     [portType|Acc];
    add_to_field(Acc, #definitions.portTypes, {portType});
process_defs_children({{wsdl, "binding"}, _Attrs, _Children}, Acc) ->
    add_to_field(Acc, #definitions.bindings, {binding,_Attrs});
%%     [binding|Acc];
process_defs_children({{wsdl, "service"}, _Attrs, _Children}, Acc) ->
    add_to_field(Acc, #definitions.services, {service}).
%%     [service|Acc].

process_types_children({{xsd,"schema"}, _, _}=SchemaNode, AccDict) ->
    Types = wsdler_xsd:parse_schema_node(SchemaNode,
                                         fun (NS,_Path) -> error({cannot_resolve_schema, NS}) end),
    wsdler_xsd:merge_schemas(Types,AccDict).
