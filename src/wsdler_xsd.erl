-module(wsdler_xsd).

%%% Purpose: Conversion of XSD DOMs into internal model.
%%% TODO: expand this descirption...

-export([parse_file/1, parse_string/1, parse_schema_node/1]).
-export([empty_schema/0, schema_to_type_list/1, merge_schemas/2]).
-export([lookup_type/2, lookup_element/2]).

-include("wsdler.hrl").
-import(wsdler_xml, [attribute/2, attribute/3, list_attribute/2, attribute/4]).

-record(schema, {
          elements :: dict(), % of #element{}
          groups :: dict(),   % of ??
          attr_groups :: dict(), % of ??
          types :: dict(),    % of typedef()
          type_order :: [qname()]
         }).
-opaque(schema() :: #schema{}).

parse_file(FileName) ->
    {ok,Text} = file:read_file(FileName),
    parse_string(Text).

parse_string(XMLText) ->
    {ok,XMLTree} = wsdler_xml:parse_string(XMLText),
    Types = parse_schema_node(XMLTree),
    {ok, Types}.

-spec empty_schema/0 :: () -> schema().
empty_schema() ->
    dict:new().

merge_schemas(TypeDict1, TypeDict2) ->
    dict:merge(no_conflicts_assumed, TypeDict1, TypeDict2).

schema_to_type_list(#schema{types=Types}) ->
    dict:to_list(Types).

lookup_type(TypeID, #schema{types=Types}) ->
    dict:fetch(TypeID, Types).

lookup_element(ElementID, #schema{elements=Elements}) ->
    dict:fetch(ElementID, Elements).

schema_to_readable(#schema{types=Types, elements=Elements}) ->
    {schema, [{types, dict:to_list(Types)},
              {elements, dict:to_list(Elements)}
             ]}. % TODO: Include other fields


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% Compiler: XSD -> internal model %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% State from phase 1:
-record(collect_state, {
          elements :: dict(), % of XML subtree
          groups :: dict(),
          attr_groups :: dict(),
          types :: dict(),
          includes_etc :: [],
          targetNS :: string()
         }).

%%% State from phase 3:
-record(refcheck_state, {
          elements :: dict(), % of XML subtree
          groups :: dict(),
          attr_groups :: dict(),
          types :: dict(),
          type_order :: [_]
         }).


parse_schema_node({{xsd,"schema"}, _, _}=SchemaNode) ->
    %% Phase 1. Flatten representation/gather definitions.
    %%          Lift nested element/group etc. up to top-level.

    State1 = collect_defs_schema_node(SchemaNode),
    debug1(State1),

    %% Phase 2. Handle imports/includes/redefines (handwaving).
    %%          Result: a type-dict?
    %%          Result: elements+groups+simple/complexTypes.

    %% TODO: Handle import/include/redefine.

    %% Phase 3. Handle hierarchies
    %%           - Order types by the partial order defined by the
    %%             type hierarchy.
    %%           - Resolve element "ref" references.

    State2 = build_type_order(State1),

    %% 4. Convert from XML to internal representation.
    %%    Check references simultaneously.

    Schema = convert_to_internal_form(State2),
    debug4(Schema),

    %% 5. Make inferences.
    %%    - Simple types:
    %%      - Determine basic-types.
    %%      - Determine joint restrictions.
    %% TODO
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
    TargetNS = wsdler_xml:attribute("targetNamespace", Attrs, undefined),
    InitState = #collect_state{elements    = dict:new(),
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
perform_collect_defs_action({add_to_dict_field, FNr, IDAttr},
                            Node={Tag,Attrs,_}, Acc) ->
    TargetNS = Acc#collect_state.targetNS,
    ID = wsdler_xml:attribute(IDAttr, Attrs, make_ref()),
    OtherAttrs = lists:keydelete(IDAttr, 1, Attrs),
    Key = if is_reference(ID) -> ID;
             true -> {TargetNS, ID}
          end,
    Old = element(FNr,Acc),
    New = dict:store(Key, Node, Old),
    Acc2 = setelement(FNr, Acc, New),
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
    [{recurse,element}, {add_to_dict_field, #collect_state.elements, "name"}];
collect_defs_action(_,"element") ->
    [{recurse,element}, {add_to_dict_field, #collect_state.elements, "<none>"}];
collect_defs_action(_,"group") ->
    [{recurse,group},   {add_to_dict_field, #collect_state.groups, "id"}];
collect_defs_action(_,"attributeGroup") ->
    [{recurse,group},   {add_to_dict_field, #collect_state.attr_groups, "id"}];
collect_defs_action(_,"simpleType") ->
    [{recurse,simpleType}, {add_to_dict_field, #collect_state.types, "name"}];
collect_defs_action(_,"complexType") ->
    [{recurse,complexType}, {add_to_dict_field, #collect_state.types, "name"}];
collect_defs_action(simpleType,"restriction") -> [];
collect_defs_action(simpleType,"list")       -> {recurse, list};
collect_defs_action(simpleType,"union")      -> {recurse, union};
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
%%%%%%%% Phase 2: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% TODO


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Phase 3: Establish partial order of types %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_type_order(#collect_state{}=State) ->
    TypeDict = State#collect_state.types,
    {_,_,TypeOrder} =
        lists:foldl(fun check_type_references/2,
                    {TypeDict, dict:new(), []},
                    dict:fetch_keys(TypeDict)),
    %% TODO: Check other references.
    #refcheck_state{
               elements    = State#collect_state.elements,
               groups      = State#collect_state.groups,
               attr_groups = State#collect_state.attr_groups,
               types       = State#collect_state.types,
               type_order  = TypeOrder
              }.

check_type_references(no_base, State) ->
    State;
check_type_references({xsd, _}, State) ->
    %% A built-in type.  Existence assumed.
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
    case strip_annotations(Children) of
        [{{xsd,"list" }, _, _}] -> no_base;
        [{{xsd,"union"}, _, _}] -> no_base;
        [{{xsd,"restriction"}, Attrs2, _}] ->
            wsdler_xml:attribute("base", Attrs2)
    end;
base_type_of({{xsd, "complexType"}, _Attrs, Children}) ->
    case Children of
        [] ->
            no_base;
        [{{xsd,"attribute"}, _, _} | _] ->
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
%%%%%%%% Phase 4: Convert to internal form %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert_to_internal_form(#refcheck_state{
                            elements = Elements,
                            groups = Groups,
                            attr_groups = AttrGroups,
                            types = Types,
                            type_order = TypeOrder
                           }=State) ->
    NewElements = convert_elements(Elements, State),
    NewTypes = convert_types(Types, State),
    #schema{elements=NewElements,
            groups=Groups,
            attr_groups=AttrGroups,
            types=NewTypes,
            type_order=TypeOrder}.

%%% References in question:
%%% - simpleType.restriction.base
%%% - complexType.simpleContent.{restriction/extension}.base
%%% - complexType.complexContent.{restriction/extension}.base
%%% - element.ref
%%% - group.ref
%%% - attributeGroup.ref

%%%========== Elements

convert_elements(Elements,State) ->
    dict:map(fun (_K,V)->convert_element(V,State) end, Elements).

convert_element({{xsd,"element"}, Attrs, Children}, State) ->
    case attribute("ref",Attrs,undefined) of
        undefined ->
            convert_element2(Attrs, Children, State);
        RefName ->
            check_element_existence(RefName, State),
            {{xsd,"element"},Attrs2,Children2} =
                dict:fetch(RefName, State#refcheck_state.elements),
            convert_element2(Attrs2,Children2, State)
    end.

convert_element2(Attrs, Children, State) ->
    ElemName = attribute("name",Attrs,undefined),
    TypeName = attribute("type",Attrs,undefined),
    io:format(user, "DB| convert_element: ~p\n", [{Attrs,Children}]),
    %% TODO: Handle <attribute> children.
    {Type,Name,Constraints} =
        case {TypeName,Children} of
            {undefined, []} ->
                io:format(user, "DB| convert_element case 1: ~p\n", [{}]),
                {{xsd,"anyType"}, ElemName, []};
            {undefined, [{ref, {xsd,Tag}, Ref,_} | Constraints0]}
              when Tag=:="simpleType";
                   Tag=:="complexType" ->
                io:format(user, "DB| convert_element case 2: ~p\n", [{}]),
                {Ref, ElemName, Constraints0};
            _ when TypeName /= undefined ->
                io:format(user, "DB| convert_element case 4: ~p\n", [{}]),
                {TypeName, ElemName, Children}
        end,
    %% TODO: Handle constraints.
    #element{name=Name, type=check_type_existence(Type,State)}.


process_element({{xsd,"element"}, Attrs, Children}) ->
    {'TODO', process_element, Attrs, Children}.
%% process_element({{xsd,"element"}, Attrs, []}) ->
%%      case attribute("ref", Attrs, none) of
%% 	none ->
%% 	    ElemName = attribute("name",Attrs),
%% 	    #element{name=ElemName}; % TODO: type
%% 	Qname ->
%% 	    #elementRef{ref=Qname} % TODO: check_element_existence(Qname)}
%%     end;
%% process_element({{xsd,"element"}, Attrs, Children}) ->
%%     ElemName = attribute("name",Attrs),
%%     case strip_annotations(Children) of
%% 	[Child] ->
%% 	    #element{name=ElemName, type=process_element_child(Child)};
%% 	[] ->
%% 	    #element{name=ElemName}
%% end.

%% process_element_child(Node={{xsd, "simpleType"},_,_}) ->
%%     process_simpleType(Node);
%% process_element_child(Node={{xsd, "complexType"},_,_}) ->
%%     process_complexType(Node).

%%%========== Groups
%%%========== Attribute groups

%%%========== Types
convert_types(Types,_State) ->
    dict:map(fun (_K,V={{xsd,"simpleType"},_,_})  -> process_simpleType(V);
                 (_K,V={{xsd,"complexType"},_,_}) -> process_complexType(V)
             end, Types).

%%%==========

check_element_existence(ElementID, #refcheck_state{elements=Dict}) ->
    dict:is_key(ElementID, Dict)
        orelse error({unresolved_element, ElementID,
                     dict:fetch_keys(Dict)}),
    ElementID.

check_group_existence(GroupID, #refcheck_state{groups=Dict}) ->
    dict:is_key(GroupID, Dict)
        orelse error({unresolved_group, GroupID,
                     dict:fetch_keys(Dict)}),
    GroupID.

check_type_existence(TypeID={xsd,_}, _) -> TypeID;
check_type_existence(TypeID, #refcheck_state{types=Dict}) ->
    dict:is_key(TypeID, Dict)
        orelse error({unresolved_type_reference, TypeID,
                      dict:fetch_keys(Dict)}),
    TypeID.

%%%=========================================================================

%%%%%% <schema> children: %%%%%%%%%%%%%%%%%%%%
%%% (include | import | redefine | annotation)*, ("schemaTop"*, annotation*)
%%% where
%%%   schemaTop ::= ( "redefinable" | element | attribute | notation)
%%%   redefinable ::= (simpleType | complexType | group | attributeGroup)
%%%
process_schema_children({{xsd,"import"}, _Attrs, _Children}, Acc, _TgtNS) ->
    Acc;
process_schema_children(E={{xsd,"element"}, _, _}, Acc, TgtNS) ->
    Element = #element{name=Name} = process_element(E),
    QName = {TgtNS, Name},
    [{QName, Element#element{name=QName}} | Acc];
process_schema_children(Node={{xsd,"simpleType"}, Attrs,_}, Acc, TgtNS) ->
    TypeName = attribute("name",Attrs),
    [{{TgtNS,TypeName},process_simpleType(Node)} | Acc];
process_schema_children(Node={{xsd,"complexType"}, Attrs, _}, Acc, TgtNS) ->
    TypeName = attribute("name",Attrs),
    io:format("DB| define type: ~s:~s\n", [TgtNS,TypeName]),
    [{{TgtNS,TypeName},process_complexType(Node)} | Acc].

process_complexType({{xsd,"complexType"}, _Attrs, Children}) ->
    {Content, Attributes} = process_complexType_children(strip_annotations(Children)),
    #complexType{content = Content, attributes=Attributes}.

%%%%%% <complexType> children: %%%%%%%%%%%%%%%%%%%%
%%% ( annotation?, "complexTypeModel")
%%% where
%%%   complexTypeModel ::= ( simpleContent | complexContent |
%%%                          ( "typeDefParticle"? "attrDecls" ) )
%%%   typeDefParticle ::= ( group | all | choice | sequence )
%%%   attrDecls ::= ( (attribute | attributeGroup)*, anyAttribute? )
%%%
process_complexType_children([]) ->
    {undefined, todo_process_complex_children} ;
process_complexType_children(Attributes=[{{xsd,"attribute"},_,_}|_]) ->
    {undefined, [process_attribute(Attr) || Attr <- Attributes]} ;
process_complexType_children([Node={{xsd,"all"},_,_} | Attributes]) ->
    {process_all(Node), [process_attribute(Attr) || Attr <- Attributes]};
process_complexType_children([Node={{xsd,"sequence"},_,_} | Attributes]) ->
    {process_sequence(Node), [process_attribute(Attr) || Attr <- Attributes]} ;
process_complexType_children([Node={{xsd,"choice"},_,_} | Attributes]) ->
    {process_choice(Node), [process_attribute(Attr) || Attr <- Attributes]} ;
process_complexType_children([{{xsd,"simpleContent"},_,[Child]}]) -> %% TODO, only one child for now
    {process_simpleContent_child(Child), []};
process_complexType_children([{{xsd,"complexContent"},_,[Child]}]) -> %% TODO, only one child for now
    {process_complexContent_child(Child), []}.

process_all({{xsd, "all"}, _, Children}) ->
    #all{content=[process_element(Child) || Child <- Children]}.

process_sequence({{xsd, "sequence"}, _Attr, Children}) ->
    #sequence{content=[process_sequence_children(Child) || Child <- Children]}.
process_sequence_children(Node={{xsd, "sequence"},_,_}) ->
    process_sequence(Node);
process_sequence_children(Node={{xsd, "all"},_,_}) ->
    process_all(Node);
process_sequence_children({ref, {xsd, "element"},Ref,Attrs}) ->
    MinOccurs = attribute("minOccurs", Attrs, fun erlang:list_to_integer/1, 1),
    MaxOccurs = attribute("maxOccurs", Attrs,
                          fun ("unbounded")->unbounded;
                              (V) -> list_to_integer(V)
                          end,
                          unbounded),
    #element_instantiation{element_ref=Ref,
                           minOccurs=MinOccurs,
                           maxOccurs=MaxOccurs};
process_sequence_children(Node={{xsd, "choice"},_,_}) ->
    process_choice(Node).

process_simpleContent_child({{xsd,"extension"},Attributes ,Children}) ->
    BaseTypeQname = attribute("base", Attributes),
    #simpleContentExtension{base = BaseTypeQname,
			    attributes = lists:map(fun(Child) -> process_attribute(Child) end, Children)}.

process_complexContent_child({{xsd,"restriction"},Attributes ,Children}) ->
    BaseTypeQname = attribute("base", Attributes),
    #complexContentRestriction{base = BaseTypeQname,
			       attributes = lists:map(fun(Child) -> process_attribute(Child) end, Children)}.

process_attribute({{xsd, "anyAttribute"},_Attributes, _Children}) ->
    {anyAttribute, todo_anyAttribute};
process_attribute({{xsd, "attribute"},Attributes, Children}) ->
    Name = attribute("name", Attributes),
    TypeName = attribute("type", Attributes, undefined),
    Use  = attribute("use", Attributes, undefined),
    Type = case {TypeName,Children} of
               {_, []} when TypeName /= undefined ->
                   TypeName;
               {undefined, [{ref,{xsd,"simpleType"},TypeRef,_Attrs}]} ->
                   TypeRef
           end,
    #attribute{name=Name, type=Type, use=Use}.


process_choice({{xsd,"choice"}, _Attr, Children}) ->
    #choice{content=[process_choice_children(Child) || Child <- Children]}.
process_choice_children(Node={{xsd, "sequence"},_,_}) ->
    process_sequence(Node);
process_choice_children(Node={{xsd, "choice"},_,_}) ->
    process_choice(Node);
process_choice_children({{xsd, "group"}, Attrs,[]}) ->
    #group{ref=attribute("ref",Attrs)};
process_choice_children(Node={{xsd, "element"},_,_}) ->
    process_element(Node).


%%%%%% <simpleType> children: %%%%%%%%%%%%%%%%%%%%
%%% (annotation?, "simpleDerivation")
%%% where
%%%   simpleDerivation ::= (restriction | list | union)
process_simpleType({{xsd, "simpleType"}, _Attr, Children}) ->
    [Child] = strip_annotations(Children),
    #simpleType{type=process_simpleType_child(Child)}.
process_simpleType_child({{xsd,"restriction"}, Attrs, Children}) ->
    process_restriction_children(attribute("base", Attrs), Children);
process_simpleType_child({{xsd,"list"}, Attrs, Children}) ->
    ItemType = case [X || X={{xsd,"simpleType"},_,_} <- Children] of
		   [] ->
		       {named,attribute("itemType", Attrs)};
		   [ItemTypeElement] ->
		       process_simpleType_child(ItemTypeElement) % ?
	       end,
    #simpleListType{itemType=ItemType};
process_simpleType_child({{xsd,"union"}, Attrs, Children}) ->
    MemberTypes = case [X || X={{xsd,"simpleType"},_,_} <- Children] of
		      [] ->
			  [{named,X}
			   || X<-list_attribute("memberTypes", Attrs)];
		      MemberTypeElements ->
			  [process_simpleType_child(strip_annotations(X))
                           || X <-MemberTypeElements]
		  end,
    #simpleUnionType{memberTypes=MemberTypes}.

process_restriction_children(Base, Children)->
        lists:foldl(fun process_restriction_child/2, #restriction{base=Base}, Children).
process_restriction_child({{xsd, "enumeration"}, Attrs, _Children}, #restriction{enumeration=EVs}=R) ->
    EnumValue = attribute("value", Attrs),
    R#restriction{enumeration=[EnumValue | EVs]};
process_restriction_child({{xsd, "pattern"}, Attrs, _Children}, #restriction{pattern=undefined}=R) ->
    Pattern = attribute("value", Attrs),
    R#restriction{pattern=Pattern};
process_restriction_child({{xsd, "minLength"}, Attrs, _Children}, #restriction{minLength=undefined}=R) ->
    Length = list_to_integer(attribute("value", Attrs)),
    R#restriction{minLength=Length};
process_restriction_child({{xsd, "maxLength"}, Attrs, _Children}, #restriction{maxLength=undefined}=R) ->
    Length = list_to_integer(attribute("value", Attrs)),
    R#restriction{maxLength=Length};
process_restriction_child({{xsd, "length"}, Attrs, _Children}, #restriction{minLength=undefined, maxLength=undefined}=R) ->
    Length = list_to_integer(attribute("value", Attrs)),
    R#restriction{minLength=Length, maxLength=Length};
process_restriction_child({{xsd, "minExclusive"}, Attrs, _Children}, #restriction{minValue=undefined}=R) ->
    Value = attribute("value", Attrs),
    R#restriction{minValue={Value,false}};
process_restriction_child({{xsd, "minInclusive"}, Attrs, _Children}, #restriction{minValue=undefined}=R) ->
    Value = attribute("value", Attrs),
    R#restriction{minValue={Value,true}};
process_restriction_child({{xsd, "maxExclusive"}, Attrs, _Children}, #restriction{maxValue=undefined}=R) ->
    Value = attribute("value", Attrs),
    R#restriction{maxValue={Value,false}};
process_restriction_child({{xsd, "maxInclusive"}, Attrs, _Children}, #restriction{maxValue=undefined}=R) ->
    Value = attribute("value", Attrs),
    R#restriction{maxValue={Value,true}};
process_restriction_child({{xsd, "fractionDigits"}, Attrs, _Children}, #restriction{}=R) ->
    Value = attribute("value", Attrs),
    R#restriction{fractionDigits=Value};
process_restriction_child({{xsd, "totalDigits"}, Attrs, _Children}, #restriction{}=R) ->
    Value = attribute("value", Attrs),
    R#restriction{totalDigits=Value}.

strip_annotations(XML) ->
    strip_annotations(XML, []).
strip_annotations([{{xsd,"annotation"}, _, _} | Rest], Acc) ->
    strip_annotations(Rest, Acc);
strip_annotations([H|R], Acc) ->
    strip_annotations(R, [H|Acc]);
strip_annotations([], Acc) ->
    lists:reverse(Acc).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% Utilities %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dict_foreach(Fun, Dict) when is_function(Fun,1) ->
    dict:fold(fun(X,_D) -> Fun(X) end, dummy, Dict).

