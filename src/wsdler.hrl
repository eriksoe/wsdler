-ifndef(wsdler_hrl).
-define(wsdler_hrl, included).

%%%========== Common stuff: ========================================
-type(ncname() :: string()). %% "non-colonized" name
-type(anyURI() :: string()).
-type(qname() :: {anyURI(), ncname()} | ncname()).

-type(erlsom_dom() :: {_,[_],[_]}).

%%%========== Simple-type related: ========================================

%% Simpletype choices:
-record(restriction,
        {base,
         pattern=undefined,
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
-record(attribute_instantiation, {attr_ref :: _}).

%% -record(elementRef, {ref :: qname()}).
%% -type(element() :: #element{} | #elementRef{}).
-type(element() :: #element_instantiation{}).
-record(attribute, {name :: ncname(),
		    type :: qname() | reference(),
		    use=optional :: optional | prohibited | required}).

%%%========== Element group related: ========================================

-record(group, {ref :: qname()}).
-record(choice,   {content :: [element_ish()]}).
-record(sequence, {content :: [element_ish()]}).
-record(all, {content :: [element_ish()]}).
-record(any, {}).
-type(element_ish() :: #element_instantiation{} | #group{} | #choice{} | #sequence{} | #all{} | #any{}).

%%%========== Complex-type related: ========================================

-record(simpleContentExtension, {base :: qname(),
				 attributes :: [#attribute{}],
                                 children :: element_ish()}).
-record(complexContentRestriction, {base :: qname(),
				    attributes :: [#attribute{}],
                                    children :: element_ish()}).
-record(complexContentExtension, {base :: qname(),
				 attributes :: [#attribute{}],
                                 children :: element_ish()}).

-record(complexType, {
          content :: #simpleContentExtension{} | #complexContentExtension{} | #complexContentRestriction{} | element_ish(), % ?
          attributes :: #attribute{}}).

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
