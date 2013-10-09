-module(wsdler_xsd_conversion).

%%% Purpose: Actual conversion of XSD DOM into internal representation.

-export([convert_to_internal_form/1]).

-include("wsdler.hrl").
-include("wsdler_xsd_internal.hrl").
-import(wsdler_xml, [attribute/2, attribute/3, attribute/4, list_attribute/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Phase 4: Convert to internal form %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert_to_internal_form(#refcheck_state{
                            elements = Elements,
                            attributes = Attributes,
                            groups = Groups,
                            attr_groups = AttrGroups,
                            types = Types,
                            type_order = TypeOrder
                           }=State) ->
    NewElements = convert_elements(Elements, State),
    NewAttributes = convert_attributes(Attributes, State),
    NewGroups = convert_groups(Groups, State),
    NewTypes = convert_types(Types, State),
    #schema{elements=NewElements,
            attributes=NewAttributes,
            groups=NewGroups,
            attr_groups=AttrGroups, % TODO: Convert
            types=NewTypes,
            type_order=TypeOrder}.

%%% References in question:
%%% - simpleType.restriction.base
%%% - complexType.simpleContent.{restriction/extension}.base
%%% - complexType.complexContent.{restriction/extension}.base
%%% - element.ref
%%% - group.ref
%%% - attributeGroup.ref

%%%======================================================================
%%%========== Conversion of Elements ====================================
%%%======================================================================

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

%%%======================================================================
%%%========== Conversion of Groups   ====================================
%%%======================================================================

convert_groups(Groups,State) ->
    io:format(user, "DB| convert_groups: ~p\n", [dict:to_list(Groups)]),
    dict:map(fun (_K,V)->convert_group(V,State) end, Groups).

%%%%%% <group> children: %%%%%%%%%%%%%%%%%%%%
%%% (annotation?, (all | choice | sequence)?)
convert_group({{xsd,"group"}, Attrs, [Child]}, State) ->
    #group{content=process_groupish(Child, State)}.

%%%======================================================================
%%%========== Conversion of Types   ====================================
%%%======================================================================

convert_types(Types,State) ->
    dict:map(fun (_K,V={{xsd,"simpleType"},_,_})  -> process_simpleType(V);
                 (_K,V={{xsd,"complexType"},_,_}) -> process_complexType(V, State)
             end, Types).

%%%========== SimpleType ========================================

%%%%%% <simpleType> children: %%%%%%%%%%%%%%%%%%%%
%%% (annotation?, "simpleDerivation")
%%% where
%%%   simpleDerivation ::= (restriction | list | union)
%%%%%% <union> children:
%%%  (annotation?, simpleType*)
%%%%%% <list> children:
%%%  (annotation?, simpleType*)
process_simpleType({{xsd, "simpleType"}, _Attr, [Child]}) ->
    #simpleType{type=process_simpleType_child(Child)}.
process_simpleType_child({{xsd,"restriction"}, Attrs, Children}) ->
    process_restriction_children(attribute("base", Attrs), Children);
process_simpleType_child({{xsd,"list"}, Attrs, Children}) ->
    %% TODO: child filtering is not necessary. Do case(@itemtype,Children).
    ItemType = case [X || X={{xsd,"simpleType"},_,_} <- Children] of
		   [] ->
		       {named,attribute("itemType", Attrs)};
		   [ItemTypeElement] ->
		       process_simpleType_child(ItemTypeElement) % ?
	       end,
    #simpleListType{itemType=ItemType};
process_simpleType_child({{xsd,"union"}, Attrs, Children}) ->
    %% TODO: child filtering is not necessary. Do case(@memberTypes,Children).
    MemberTypes = case [X || X={{xsd,"simpleType"},_,_} <- Children] of
		      [] ->
			  [{named,X}
			   || X<-list_attribute("memberTypes", Attrs)];
		      MemberTypeElements ->
			  [process_simpleType_child(X)
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

%%%========== ComplexType ========================================

process_complexType({{xsd,"complexType"}, _Attrs, Children}, State) ->
    ComplexTypeDef = process_complexType_children(Children, State),
    #complexType{content = ComplexTypeDef}.

%%%%%% <complexType> children: %%%%%%%%%%%%%%%%%%%%
%%% ( annotation?, "complexTypeModel")
%%% where
%%%   complexTypeModel ::= ( simpleContent | complexContent |
%%%                          ( "typeDefParticle"? "attrDecls" ) )
%%%   typeDefParticle ::= ( group | all | choice | sequence )
%%%   attrDecls ::= ( (attribute | attributeGroup)*, anyAttribute? )
%%%
-spec process_complexType_children/2 :: ([erlsom_dom()], #refcheck_state{}) ->complexTypeDef().
process_complexType_children([{{xsd,"simpleContent"},_,Children}], State) ->
    process_simpleContent_children(Children, State);
process_complexType_children([{{xsd,"complexContent"},_,Children}], State) ->
    process_complexContent_children(Children, State);
process_complexType_children(Children, State) ->
    process_complexContentTypeModel(Children, State).

%%% Quoth the spec, about "complexTypeModel":
%%%   This branch is short for
%%%   <complexContent>
%%%     <restriction base="xs:anyType">...</restriction>
%%%   </complexContent>
process_complexContentTypeModel(Children, State) ->
    SynthNode = {{xsd,"restriction"},
                 [{{[],"base"}, {xsd,"anyType"}}],
                 Children},
    process_complexContent_children([SynthNode], State).

%%%%%% <complexContent> children: %%%%%%%%%%%%%%%%%%%%
%%   (annotation?, (restriction | extension))
%%
%%%%%% <restriction> children: %%%%%%%%%%%%%%%%%%%%
%%   (annotation?, (group | all | choice | sequence)?, ((attribute | attributeGroup)*, anyAttribute?))
%%
%%%%%% <extension> children: %%%%%%%%%%%%%%%%%%%%
%%   (annotation?, ((group | all | choice | sequence)?, ((attribute | attributeGroup)*, anyAttribute?)))
%% </extension>
process_complexContent_children([{{xsd,Tag}, Attributes, Children}], State)
  when Tag =:= "restriction";
       Tag =:= "extension" ->
    BaseTypeQname = attribute("base", Attributes),
    case Children/=[] andalso is_groupish(hd(Children)) of
        true ->
            ElemContents = process_groupish(hd(Children), State),
            AttrChildren = tl(Children);
        false ->
            ElemContents = [],
            AttrChildren = Children
    end,
    AttrContents = lists:map(fun(Child) -> process_attribute(Child, State) end, AttrChildren),
    case Tag of
        "restriction" ->
            #complexContentRestriction{base = BaseTypeQname,
                                       attributes = AttrContents,
                                       children = ElemContents};
        "extension" ->
            #complexContentExtension{base = BaseTypeQname,
                                     attributes = AttrContents,
                                     children = ElemContents}
    end.

%%%%%% <simpleContent> children:
%%% (annotation?, (restriction | extension))
process_simpleContent_children([{{xsd,"extension"}, Attributes,Children}],
                               State) ->
    BaseTypeQname = attribute("base", Attributes),
    #simpleContentExtension{base = BaseTypeQname,
			    attributes = lists:map(fun(Child) -> process_attribute(Child, State) end, Children)};
process_simpleContent_children([{{xsd,"restriction"}, Attributes, Children}],
                               State) ->
    BaseTypeQname = attribute("base", Attributes),
    %% TODO: Handle type and facets!
    #simpleContentRestriction{base = BaseTypeQname,
                              type = 'TODO',
                              facets = 'TODO',
                              attributes = lists:map(fun(Child) -> process_attribute(Child, State) end, Children)}.

-spec process_attribute/2 :: (erlsom_dom(), #refcheck_state{}) -> attribute_ish().
process_attribute({ref, {xsd, "attribute"}, Ref, Attributes}, State) ->
    Use  = list_to_atom(attribute("use", Attributes, "undefined")),
    #attribute_instantiation{ref=check_attribute_existence(Ref,State), use=Use};
process_attribute({ref, {xsd, "attributeGroup"}, Ref, _Attributes}, State) ->
    #attributeGroup_ref{ref=check_attributeGroup_existence(Ref,State)};
process_attribute({{xsd, "anyAttribute"},_Attributes, _Children}, _State) ->
    any_attribute.

%%%========== Element and element groups ("groupish")

process_element_or_groupish({ref, {xsd, "element"},_,_}=Node, State) ->
    process_element(Node, State);
process_element_or_groupish(Node, State) ->
    process_groupish(Node, State).

process_element({ref, {xsd, "element"},Ref,Attrs}, State) ->
    MinOccurs = attribute("minOccurs", Attrs, fun erlang:list_to_integer/1, 1),
    MaxOccurs = attribute("maxOccurs", Attrs,
                          fun ("unbounded")->unbounded;
                              (V) -> list_to_integer(V)
                          end,
                          unbounded),
    #element_instantiation{element_ref=check_element_existence(Ref,State),
                           minOccurs=MinOccurs,
                           maxOccurs=MaxOccurs}.

is_groupish({ref, {xsd, "attribute"},_,_}) -> false;
is_groupish({{xsd, Tag},_,_}) ->
    Tag=:="choice" orelse
    Tag=:="group" orelse
    Tag=:="sequence" orelse
    Tag=:="all" orelse
    Tag=:="any".
process_groupish(Node={{xsd, "sequence"},_,_}, State) ->
    process_sequence(Node, State);
process_groupish(Node={{xsd, "all"},_,_}, State) ->
    process_all(Node, State);
process_groupish(Node={{xsd, "choice"},_,_}, State) ->
    process_choice(Node, State);
process_groupish({ref, {xsd, "group"}, Ref, _Attrs}, State) ->
    #group_instantiation{ref=check_group_existence(Ref,State)};
process_groupish(Node={{xsd, "element"},_,_}, State) ->
    process_element(Node, State);
process_groupish({{xsd, "any"},_,_}, _State) ->
    #any{}.


process_all({{xsd, "all"}, _, Children}, State) ->
    #all{content=[process_element(Child, State) || Child <- Children]}.

process_sequence({{xsd, "sequence"}, _Attrs, Children}, State) ->
    #sequence{content=[process_element_or_groupish(Child, State)
                       || Child <- Children]}.
process_choice({{xsd,"choice"}, _Attr, Children}, State) ->
    #choice{content=[process_element_or_groupish(Child, State)
                     || Child <- Children]}.

%%%========== Attribute-ish

is_attributeish({{xsd, Tag},_,_}) ->
    Tag=:="attribute" orelse
    Tag=:="attributeGroup" orelse
    Tag=:="anyAttribute".

%%%======================================================================
%%%========== Conversion of Attributes ==================================
%%%======================================================================

convert_attributes(Attributes, State) ->
    io:format(user, "DB| convert_attributes: ~p\n", [dict:to_list(Attributes)]),
    dict:map(fun (_K,V)->convert_attribute(V,State) end, Attributes).

convert_attribute({{xsd, "attribute"}, Attributes, Children}, State) ->
    %% 3 If the item's parent is not <schema>, then all of the following must be true:
    %% 3.1 One of ref or name must be present, but not both.
    %% 3.2 If ref is present, then all of <simpleType>, form and type must be absent.
    %% 4 type and <simpleType> must not both be present.
    Ref = attribute("ref", Attributes, undefined),
    Name = attribute("name", Attributes, undefined),
    TypeName = attribute("type", Attributes, undefined),
    Use  = attribute("use", Attributes, undefined),
    Type = case {Ref,TypeName,Children} of
               {_, undefined, []} when Ref /= undefined ->
                   {'TODO', ref, Ref};
               {undefined, _, []} when TypeName /= undefined ->
                   TypeName;
               {undefined, undefined, [{ref,{xsd,"simpleType"},TypeRef,_Attrs}]} ->
                   TypeRef
           end,
    #attribute{name=Name, type=check_type_existence(Type,State), use=Use}.

%%%======================================================================
%%%========== Conversion of Attribute groups ============================
%%%======================================================================


%%%============================== Utilities ==============================

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

check_attribute_existence(AttributeID, #refcheck_state{attributes=Dict}) ->
    dict:is_key(AttributeID, Dict)
        orelse error({unresolved_attribute, AttributeID,
                     dict:fetch_keys(Dict)}),
    AttributeID.

check_attributeGroup_existence(AttributeGroupID, #refcheck_state{attr_groups=Dict}) ->
    dict:is_key(AttributeGroupID, Dict)
        orelse error({unresolved_attributeGroup, AttributeGroupID,
                     dict:fetch_keys(Dict)}),
    AttributeGroupID.

check_type_existence(TypeID={xsd,_}, _) -> TypeID;
check_type_existence(TypeID, #refcheck_state{types=Dict}) ->
    dict:is_key(TypeID, Dict)
        orelse error({unresolved_type_reference, TypeID,
                      dict:fetch_keys(Dict)}),
    TypeID.

