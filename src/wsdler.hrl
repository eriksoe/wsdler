-ifndef(wsdler_hrl).
-define(wsdler_hrl, included).

%%%========== Common stuff: ========================================
-type(ncname() :: string()). %% "non-colonized" name
-type(anyURI() :: string()).
-type(namespace() :: atom() | anyURI()).
-type(qname() :: {namespace(), ncname()}).

-type(erlsom_dom() :: {_,[_],[_]}).
-type(and_or_tree(T) :: T
                      | {'and', and_or_tree(T), and_or_tree(T)}
                      | {'or', and_or_tree(T), and_or_tree(T)}).

%%%========== Simple-type related: ========================================

%% Simpletype choices:
-record(restriction,
        {primitive=undefined,
         base,
         pattern=undefined :: undefined | and_or_tree(string()),
         enumeration=[],
         minLength=undefined,
         maxLength=undefined,
         minValue=undefined, % {Value,Inclusive}
         maxValue=undefined, % {Value,Inclusive}
         fractionDigits=undefined,
         totalDigits=undefined
        }).
-record(simpleListType,  {itemType}).
-record(simpleUnionType, {memberTypes}).
-type(simpleDerivation() :: #restriction{} | #simpleListType{} | #simpleUnionType{}).

-record(simpleType, {type :: {named,_} | simpleDerivation()}).

%%%========== Element related: ========================================
-record(element, {name :: _,
		  type :: typedef()}).
-record(element_instantiation, {element_ref :: _,
                                minOccurs :: integer(),
                                maxOccurs :: integer() | unbounded}).

-type(element() :: #element_instantiation{}).
-record(attribute, {name :: ncname(),
		    type :: qname(),
		    use=optional :: optional | prohibited | required}).
-record(attribute_instantiation, {
          ref :: qname() | reference(),
          use=optional :: optional | prohibited | required | undefined}).

%%%========== Element group related: ========================================

-record(group_instantiation, {ref :: qname()}).
-record(group,    {content :: [element_ish()]}).
-record(choice,   {content :: [element_ish()]}).
-record(sequence, {content :: [element_ish()]}).
-record(all, {content :: [element_ish()]}).
-record(any, {}).
-type(element_ish() :: #element_instantiation{} | #group{} | #choice{} | #sequence{} | #all{} | #any{}).

%%%========== Attribute group related: ========================================

-record(attributeGroup_ref, {ref :: qname() | reference()}).
-type(attribute_ish() :: #attribute_instantiation{} | #attributeGroup_ref{} | any_attribute).

%%%========== Complex-type related: ========================================

-record(simpleContentRestriction, {base :: qname(),
                                   type :: qname(),
                                   facets :: [#restriction{}],
                                   attributes :: [attribute_ish()]}).
-record(simpleContentExtension, {base :: qname(),
				 attributes :: [attribute_ish()]}).
-record(complexContentRestriction, {base :: qname(),
                                    children :: element_ish(),
                                    attributes :: [attribute_ish()]}).
-record(complexContentExtension, {base :: qname(),
                                  children :: element_ish(),
                                  attributes :: [attribute_ish()]}).

-record(complexType, { content :: complexTypeDef()}).

-type( complexTypeDef() :: #simpleContentRestriction{}
                         | #simpleContentExtension{}
                         | #complexContentExtension{}
                         | #complexContentRestriction{}).

%%%========== XSD related: ========================================

-type(typedef() :: #simpleType{} | #complexType{}).

%%%========== WSDL related: ========================================

-record(definitions, {
          types,
          messages=[],
          portTypes=[],
          bindings=[],
          services=[]
         }).
%% -record(types, {
%%           types=[]
%%          }).


-record(wsdl, {
          typedict :: dict:dict(_,_)
                      }).

-endif.
