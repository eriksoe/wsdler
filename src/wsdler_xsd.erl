-module(wsdler_xsd).

%%% Purpose: Conversion of XSD DOMs into internal model.
%%% TODO: expand this description...

-export([parse_file/1, parse_string/1, parse_string/2, parse_schema_node/2]).
-export([empty_schema/0, schema_to_type_list/1, schema_to_element_list/1, merge_schemas/2]).
-export([lookup_type/2, lookup_element/2, lookup_attribute/2]).

-include("wsdler.hrl").
-include("wsdler_xsd_internal.hrl").
-import(wsdler_xml, [attribute/2, attribute/3, list_attribute/2, attribute/4]).

-opaque(schema() :: #schema{}).

%%% Namespaces:
-define(XML_NS, "http://www.w3.org/XML/1998/namespace").
-define(XSD_NS, "http://www.w3.org/2001/XMLSchema").

parse_file(FileName) ->
    {ok, Here} = file:get_cwd(),
    ResolverFun0 = file_resolver(Here),
    {Tree, ResolverFun} = ResolverFun0(dummy,FileName),
    Types = parse_schema_node(Tree, ResolverFun),
    {ok, Types}.

parse_string(XMLText) ->
    parse_string(XMLText, fun (NS,_Path) -> error({cannot_resolve_schema, NS}) end).

parse_string(XMLText, ResolverFun) when is_function(ResolverFun,2) ->
    Types = parse_schema_node(xml_to_tree(XMLText), ResolverFun),
    {ok, Types}.

file_resolver(FileName) ->
    OrgAbsName = filename:absname(FileName),
    fun(_NS,Path) ->
            io:format(user, "DB| file_resolver: resolve ~p / ~p (ns=~p)\n",
                      [OrgAbsName, Path, _NS]),
            %% TODO: Check relativeness of path
            AbsIncluded = filename:absname_join(OrgAbsName, Path),
            {file_to_tree(AbsIncluded), file_resolver(filename:dirname(AbsIncluded))}
    end.

file_to_tree(FileName) ->
    {ok,Text} = file:read_file(FileName),
    xml_to_tree(Text).

xml_to_tree(XMLText) ->
    {ok,XMLTree} = wsdler_xml:parse_string(XMLText),
    XMLTree.

-spec empty_schema/0 :: () -> schema().
empty_schema() ->
    dict:new().

merge_schemas(TypeDict1, TypeDict2) ->
    dict:merge(no_conflicts_assumed, TypeDict1, TypeDict2).

schema_to_type_list(#schema{types=Types, targetNS=TargetNS}) ->
    [KV || KV={{KNS,_},_} <- dict:to_list(Types),
           KNS =:= TargetNS].

schema_to_element_list(#schema{elements=Elements}) ->
    dict:to_list(Elements).

lookup_type(TypeID, #schema{types=Types}) ->
    dict:fetch(TypeID, Types).

lookup_element(ElementID, #schema{elements=Elements}) ->
    dict:fetch(ElementID, Elements).

lookup_attribute(AttrID, #schema{attributes=Attrs}) ->
    dict:fetch(AttrID, Attrs).

schema_to_readable(#schema{}=Schema) ->
    {schema, [{types, dict:to_list(Schema#schema.types)},
              {groups, dict:to_list(Schema#schema.groups)},
              {elements, dict:to_list(Schema#schema.elements)},
              {attributes, dict:to_list(Schema#schema.attributes)},
              {attr_groups, dict:to_list(Schema#schema.attr_groups)}
             ]}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% Compiler: XSD -> internal model %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%% <schema> children: %%%%%%%%%%%%%%%%%%%%
%%% (include | import | redefine | annotation)*, ("schemaTop"*, annotation*)
%%% where
%%%   schemaTop ::= ( "redefinable" | element | attribute | notation)
%%%   redefinable ::= (simpleType | complexType | group | attributeGroup)
%%%
parse_schema_node({{xsd,"schema"}, _, _}=SchemaNode, ResolverFun)
  when is_function(ResolverFun,2) ->
    %% Phase 1. Flatten representation/gather definitions.
    %%          Lift nested element/group etc. up to top-level.
    %%          Result: a #collect_state{}.

    State1 = collect_defs_schema_node(SchemaNode),
%%    debug1(State1),

    %% Phase 2. Handle imports/includes/redefines.
    %%          Result: a #collect_state{}, supplemented with imported/
    %%          included definitions.

    State2 = handle_imports(State1, ResolverFun),

    %% Phase 3. Handle hierarchies
    %%           - Order types by the partial order defined by the
    %%             type hierarchy.
    %%           - Resolve element "ref" references.

    State3 = build_type_order(State2),

    %% 4. Convert from XML to internal representation.
    %%    Check references simultaneously.

    State4 = wsdler_xsd_conversion:convert_to_internal_form(State3),
%%    debug4(Schema),

    %% 5. Make inferences.
    %%    - Simple types:
    %%      - Determine basic-types.
    %%      - Determine joint restrictions.
    %% TODO
    Schema = handle_inheritance(State4),

    Schema.


debug1(State) ->
    io:format(user, "DB| Phase 1 output: ~p\n",
              [[if element(1,X)==dict ->
                        dict:to_list(X);
                   true -> X
                end || X <- tuple_to_list(State)]]).

debug4(Schema) ->
    io:format(user, "DB| Phase 3 output: ~p\n", [schema_to_readable(Schema)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Phase 1: Collect definitions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

collect_defs_schema_node({{xsd,"schema"}, Attrs, _Children}=E) ->
    TargetNS = translate_namespace_uri(wsdler_xml:attribute("targetNamespace", Attrs, no_ns)),
    InitState = #collect_state{elements    = dict:new(),
                               attributes  = dict:new(),
                               groups      = dict:new(),
                               attr_groups = dict:new(),
                               types       = dict:new(),
                               includes_etc = [],
                               targetNS    = TargetNS},
    {_,Result} = do_collect_defs(root, E, InitState),
    Result.

%%% Tree state machine engine:
do_collect_defs(StateName, {{xsd,Tag}, _Attrs, _Children}=E, Acc) ->
    perform_collect_defs_action(collect_defs_action(StateName,Tag), E, Acc).

perform_collect_defs_action([Action|Actions], Node, Acc) ->
    {Node2, Acc2} = perform_collect_defs_action(Action, Node, Acc),
    perform_collect_defs_action(Actions, Node2, Acc2);
perform_collect_defs_action([], Node, Acc) ->
    {Node, Acc};
perform_collect_defs_action(ignore, _Node, Acc) ->
    {removed, Acc};
perform_collect_defs_action(add_to_includes_etc, Node, Acc) ->
    OldList = Acc#collect_state.includes_etc,
    {removed, Acc#collect_state{includes_etc=[Node | OldList]}};
perform_collect_defs_action({register_in_dict, FNr, IDAttr},
                            Node={Tag,Attrs,_}, Acc) ->
    TargetNS = Acc#collect_state.targetNS,
    undefined = wsdler_xml:attribute("ref", Attrs, undefined),
    ID = wsdler_xml:attribute(IDAttr, Attrs),
    OtherAttrs = lists:keydelete(IDAttr, 1, Attrs),
    Key = if is_reference(ID) -> ID;
             true -> {TargetNS, ID}
          end,
    Old = element(FNr,Acc),
    New = dict:store(Key, Node, Old),
    Acc2 = setelement(FNr, Acc, New),
    {removed, Acc2};
perform_collect_defs_action({refer_to_dict, FNr},
                            Node={Tag,Attrs,_}, Acc) ->
    TargetNS = Acc#collect_state.targetNS,
    case wsdler_xml:attribute("ref", Attrs, undefined) of
        undefined ->
            ID = make_ref(),
            OtherAttrs = Attrs,
            Key = if is_reference(ID) -> ID;
                     true -> {TargetNS, ID}
                  end,
            Old = element(FNr,Acc),
            New = dict:store(Key, Node, Old),
            Acc2 = setelement(FNr, Acc, New);
        RefAttr ->
            Key = RefAttr,
            OtherAttrs = lists:keydelete("ref", 1, Attrs),
            Acc2 = Acc
    end,
    {{ref,Tag,Key,OtherAttrs}, Acc2};
perform_collect_defs_action({recurse, RecStateName}, {Tag,Attrs,Children}, Acc) ->
    {Children2,Acc2} = lists:mapfoldl(fun(C,A) ->
                                       do_collect_defs(RecStateName, C, A)
                               end,
                               Acc,
                               Children),
    Node2 = {Tag, Attrs, [C || C <- Children2, C /= removed]},
    {Node2, Acc2}.


%%% Tree state machine definition:
collect_defs_action(root,"schema")     -> {recurse, schema};
collect_defs_action(_,"annotation")    -> ignore;
collect_defs_action(schema,"import")   -> add_to_includes_etc;
collect_defs_action(schema,"include")  -> add_to_includes_etc;
collect_defs_action(schema,"redefine") -> add_to_includes_etc;
collect_defs_action(schema,"element") ->
    [{recurse,element}, {register_in_dict, #collect_state.elements, "name"}];
collect_defs_action(_,"element") ->
    [{recurse,element}, {refer_to_dict, #collect_state.elements}];
collect_defs_action(schema,"attribute") ->
    [{recurse,attribute}, {register_in_dict, #collect_state.attributes, "name"}];
collect_defs_action(_,"attribute") ->
    [{recurse,attribute}, {refer_to_dict, #collect_state.attributes}];
collect_defs_action(schema,"group") ->
    [{recurse,group},   {register_in_dict, #collect_state.groups, "name"}];
collect_defs_action(_,"group") ->
    [{recurse,group},   {refer_to_dict, #collect_state.groups}];
collect_defs_action(schema,"attributeGroup") ->
    [{recurse,group},   {register_in_dict, #collect_state.attr_groups, "name"}];
collect_defs_action(_,"attributeGroup") ->
    [{recurse,group},   {refer_to_dict, #collect_state.attr_groups}];
collect_defs_action(schema,"simpleType") ->
    [{recurse,simpleType}, {register_in_dict, #collect_state.types, "name"}];
collect_defs_action(_,"simpleType") ->
    [{recurse,simpleType}, {refer_to_dict, #collect_state.types}];
collect_defs_action(schema,"complexType") ->
    [{recurse,complexType}, {register_in_dict, #collect_state.types, "name"}];
collect_defs_action(_,"complexType") ->
    [{recurse,complexType}, {refer_to_dict, #collect_state.types}];
collect_defs_action(simpleType,"restriction") -> {recurse, simpleRestriction};
collect_defs_action(simpleType,"list")       -> {recurse, list};
collect_defs_action(simpleType,"union")      -> {recurse, union};
collect_defs_action(simpleRestriction,_) -> [];
collect_defs_action(complexType,"simpleContent") -> {recurse,simpleContent};
collect_defs_action(complexType,"complexContent") -> {recurse,complexContent};
collect_defs_action(complexType,"all") -> {recurse,group};
collect_defs_action(complexType,"choice")  -> {recurse,group};
collect_defs_action(complexType,"sequence")  -> {recurse,group};
collect_defs_action(complexType,"attributeGroup") -> {recurse,attributeGroup};
collect_defs_action(complexType,"anyAttribute") -> [];
collect_defs_action(_,"attribute") -> {recurse,attribute};
collect_defs_action(simpleContent,"extension")    -> {recurse,simple_extension};
collect_defs_action(complexContent,"restriction") -> {recurse,group};
collect_defs_action(complexContent,"extension") -> {recurse,group};
collect_defs_action(group,"sequence") -> {recurse,group};
collect_defs_action(group,"choice")   -> {recurse,group};
collect_defs_action(group,"all")      -> {recurse,group};
collect_defs_action(group,"any")      -> {recurse,group};
collect_defs_action(element,"key") -> [];
collect_defs_action(element,"unique") -> [];
collect_defs_action(element,"keyref") -> [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Phase 2: Handle imports %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_imports(State=#collect_state{includes_etc=Directives}, ResolverFun) ->
    ImplicitsResolverFun = fun implicit_imports_resolver/2,
    %% Add implicit import:
    ImplicitInclude1 = {{xsd, "import"},
                        [{{"","namespace"}, ?XML_NS}], []},
    ImplicitInclude2 = {{xsd, "import"},
                        [{{"","namespace"}, {synthetic,xsd}}], []},
    Directives2 = [{ImplicitInclude1, ImplicitsResolverFun},
                   {ImplicitInclude2, ImplicitsResolverFun}]
        ++ [{D, ResolverFun} || D <- Directives],
    handle_imports_until_fixedpoint(State, Directives2, dict:new()).

handle_imports_until_fixedpoint(State, [], _NSSet) ->
    State;
handle_imports_until_fixedpoint(State, [Directive | Rest], NSSet) ->
    case Directive of
        {{{xsd, "import"}, Attrs, []}, ResolverFun} ->
            Namespace = translate_namespace_uri(attribute("namespace", Attrs)),
            case dict:is_key(Namespace, NSSet) of
                true -> % Already imported
                    NSSet2 = NSSet,
                    Rest2  = Rest,
                    State2 = State;
                false -> % Do import
                    NSSet2 = dict:store(Namespace, dummy, NSSet),
                    SchemaLocation = attribute("schemaLocation", Attrs, undefined),
                    {ImportedTree,ResolverForImported} =
                        ResolverFun(Namespace, SchemaLocation),
                    {State2,MoreDirectives} = check_and_merge_import(ImportedTree, ResolverForImported, Namespace, State),
                    Rest2 = MoreDirectives ++ Rest
            end;
        {{{xsd, "include"}, Attrs, []}, ResolverFun} ->
            SchemaLocation = attribute("schemaLocation", Attrs),
            Key = {inclusion_into,State#collect_state.targetNS, erlang:fun_info(ResolverFun), SchemaLocation},
            case dict:is_key(Key, NSSet) of
                true -> % Already included
                    NSSet2 = NSSet,
                    Rest2  = Rest,
                    State2 = State;
                false ->
                    NSSet2 = dict:store(Key, dummy, NSSet),
                    {IncludedTree,ResolverForIncluded} =
                        ResolverFun(include, SchemaLocation),
                    {State2,MoreDirectives} = check_and_merge_include(IncludedTree, ResolverForIncluded, State),
                    Rest2 = MoreDirectives ++ Rest
            end
            %% TODO: Handle redefine
    end,
    handle_imports_until_fixedpoint(State2, Rest2, NSSet2).

check_and_merge_import(ImportedTree, ResolverForImported, ExpectedNS, State) ->
    ImportedDefs = collect_defs_schema_node(ImportedTree),
    check_namespace(ImportedDefs, ExpectedNS),
    merge_collected_defs(State, ImportedDefs, ResolverForImported).

check_and_merge_include(IncludedTree, ResolverForIncluded, State) ->
    {{xsd,"schema"}, InclAttrs, InclChildren} = IncludedTree,
    ActualNS = attribute("namespace", InclAttrs, undefined),
    IncluderNS = State#collect_state.targetNS,

    Tree2 = case ActualNS of
                NS when NS=:=IncluderNS ->
                    IncludedTree;
                undefined ->
                    InclAttrs2 = [{{"","namespace"}, IncluderNS} | InclAttrs],
                    {{xsd,"schema"}, InclAttrs2, InclChildren}
            end,
    IncludedDefs = collect_defs_schema_node(Tree2),
    merge_collected_defs(State, IncludedDefs, ResolverForIncluded).

merge_collected_defs(Main, ToAdd, ResolverFun) ->
    ConflictFun = fun (Key,V1,V2) -> error({key_conflict_on_import,
                                            [{key,Key}, {value1, V1}, {value2,V2}]})
                  end,
    Elements = dict:merge(ConflictFun, Main#collect_state.elements, ToAdd#collect_state.elements),
    Attributes = dict:merge(ConflictFun, Main#collect_state.attributes, ToAdd#collect_state.attributes),
    Groups = dict:merge(ConflictFun, Main#collect_state.groups, ToAdd#collect_state.groups),
    AttrGroups = dict:merge(ConflictFun, Main#collect_state.attr_groups, ToAdd#collect_state.attr_groups),
    Types = dict:merge(ConflictFun, Main#collect_state.types, ToAdd#collect_state.types),
    Directives = [{D,ResolverFun} || D <- ToAdd#collect_state.includes_etc],

    Merged =
        Main#collect_state{elements    = Elements,
                           attributes  = Attributes,
                           groups      = Groups,
                           attr_groups = AttrGroups,
                           types       = Types},
    {Merged,Directives}.

check_namespace(#collect_state{targetNS=ActualNS}, ExpectedNS) ->
    case ExpectedNS of
        X when X=:=ActualNS -> ok;
        {synthetic,X} when X=:=ActualNS -> ok;
        _ -> error({import_has_wrong_namespace, ActualNS, ExpectedNS})
    end.

implicit_imports_resolver(?XML_NS, _Path) ->
    FileName = filename:join(code:priv_dir(wsdler), "xml.xsd"),
    {file_to_tree(FileName), dummy};
implicit_imports_resolver({synthetic,xsd}, _Path) ->
    FileName = filename:join(code:priv_dir(wsdler), "xsd-types.xsd"),
    {file_to_tree(FileName), dummy}.

merge_import(ImportedTree, ResolverForImported, Namespace, State) ->
    %% TODO!
    State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Phase 3: Establish partial order of types %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_type_order(#collect_state{}=State) ->
    TypeDict = State#collect_state.types,
    {_,_,RevTypeOrder} =
        lists:foldl(fun check_type_references/2,
                    {TypeDict, dict:new(), []},
                    dict:fetch_keys(TypeDict)),
    TypeOrder = lists:reverse(RevTypeOrder),
    %% TODO: Check other references.
    #refcheck_state{
               elements    = State#collect_state.elements,
               attributes  = State#collect_state.attributes,
               groups      = State#collect_state.groups,
               attr_groups = State#collect_state.attr_groups,
               types       = State#collect_state.types,
               targetNS    = State#collect_state.targetNS,
               type_order  = TypeOrder
              }.

check_type_references(no_base, State) ->
    State;
check_type_references({xsd,"anySimpleType"}, State) ->
    %% Implicit root type for all simple types.
    State;
check_type_references(Key, {TypeDict, TypeStates, Acc}=State) ->
    case dict:find(Key, TypeStates) of
        {ok, done}     -> State;
        {ok, visiting} -> error({cycle_in_type_hiearchy, Key});
        error ->
            case dict:find(Key, TypeDict) of
                error ->
                    error({unresolved_type_reference, Key, dict:fetch_keys(TypeDict)});
                {ok, TypeNode} ->
                    %% {_Tag, Attrs, _Children} = Type,
                    BaseType = base_type_of(TypeNode), %wsdler_xml:attribute("base", Attrs),
                    TypeStates2 = dict:store(Key, visiting, TypeStates),
                    {_, TypeStates3, Acc2} =
                        check_type_references(BaseType,
                                              {TypeDict, TypeStates2, Acc}),
                    TypeStates4 = dict:store(Key, done, TypeStates3),
                    {TypeDict, TypeStates4, [Key | Acc2]}
            end
    end.

base_type_of({{xsd, "simpleType"}, _Attrs, Children}) ->
    case Children of
        [{{xsd,"list" }, _, _}] -> no_base;
        [{{xsd,"union"}, _, _}] -> no_base;
        [{{xsd,"restriction"}, _Attrs2,
          [{ref, {xsd,"simpleType"}, BaseKey, _Attrs3} | _]
         }] ->
            BaseKey;
        [{{xsd,"restriction"}, Attrs2, _}] ->
            wsdler_xml:attribute("base", Attrs2)
    end;
base_type_of({{xsd, "complexType"}, _Attrs, Children}) ->
    case Children of
        [] ->
            no_base;
        [{ref, {xsd,"attribute"}, _, _} | _] ->
            no_base;
        [{{xsd,Tag}, _, _} | _] when Tag=:="group";
                                     Tag=:="all";
                                     Tag=:="choice";
                                     Tag=:="sequence" ->
            no_base;
        [{{xsd,"simpleContent"}, _,
          [{{xsd,Tag}, Attrs2, _}]}] when Tag=:="restriction";
                                         Tag=:="extension" ->
            wsdler_xml:attribute("base", Attrs2);
        [{{xsd,"complexContent"}, _,
          [{{xsd,Tag}, Attrs2, _}]}] when Tag=:="restriction";
                                          Tag=:="extension" ->
            wsdler_xml:attribute("base", Attrs2)
    end;
base_type_of(Other) -> error({incomplete, base_type_of, Other}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Phase 5: Handle inheritance %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_inheritance(#schema{types=Types, type_order=TypeOrder}=State) ->
    OldTypesList = lists:map(fun(TypeKey) ->
                                     {TypeKey,dict:fetch(TypeKey, Types)} end,
                             TypeOrder),
    NewTypes = lists:foldl(fun({TypeKey,Type},TypesAcc) ->
                                   Type2 = handle_type_inheritance(TypeKey, Type, TypesAcc),
                                   dict:store(TypeKey,Type2, TypesAcc)
                           end,
                           dict:new(),
                           OldTypesList),
    State#schema{types=NewTypes}.

handle_type_inheritance(TypeKey, Type, BaseTypeDict) ->
    case Type of
        #simpleType{type=#restriction{base=BaseKey}=Restriction} ->
            case is_primitive_type(TypeKey) of
                {yes, PrimType} -> ok;
                no -> PrimType = primitive_type_of(BaseKey, BaseTypeDict)
            end,
            %% TODO: Compute sum of restrictions
            NewRestriction = Restriction#restriction{
                               primitive=PrimType % Inherited
                              },
            Type#simpleType{type=NewRestriction};
        #simpleType{} -> Type; % List or union type
        #complexType{} -> Type
    end.

primitive_type_of(TypeKey, TypeDict) ->
    case dict:find(TypeKey, TypeDict) of
        {ok,#simpleType{type=#restriction{primitive=PrimType}}} when PrimType /= undefined ->
            PrimType;
        {ok,#simpleType{type=#simpleListType{}}} ->
            list;
        {ok,#simpleType{type=#simpleUnionType{}}} ->
            union;
        error ->
            error({"primitive_type_of(~p): type not found among ~p (is_prim=~p)",
                   [TypeKey, dict:fetch_keys(TypeDict), is_primitive_type(TypeKey)]})
    end.

is_primitive_type({xsd,Name}) ->
    case lists:member(Name,
                      ["anySimpleType",
                       "string", "boolean",
                       "decimal", "double", "float",
                       "hexBinary", "base64Binary",
                       "NOTATION", "QName", "anyURI",
                       "duration", "dateTime", "date", "time",
                       "gDay", "gMonth", "gYear", "gMonthDay", "gYearMonth"])
    of
        true  -> {yes, list_to_atom(Name)};
        false -> no
    end;
is_primitive_type(_) ->
    no.


translate_namespace_uri(?XSD_NS) -> xsd;
translate_namespace_uri(NS) -> NS.
