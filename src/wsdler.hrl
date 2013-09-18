-ifndef(wsdler_hrl).
-define(wsdler_hrl, included).

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
-record(element, {name :: _,
		  type :: typedef()}).
-record(element_instantiation, {element :: #element{},
                                minOccurs :: integer(),
                                maxOccurs :: integer() | unbounded}).

%% -record(elementRef, {ref :: qname()}).
%% -type(element() :: #element{} | #elementRef{}).
-type(element() :: #element_instantiation{}).
-record(attribute, {name :: ncname(),
		    type :: qname(),
		    use=optional :: optional | prohibited | required,
		    simpleType :: #simpleType{}}).

-type(ncname() :: string()). %% "non-colonized" name
-type(anyURI() :: string()).
-type(qname() :: {anyURI(), ncname()} | ncname()).
-record(complexContentRestriction, {base :: qname(),
				    attributes :: [#attribute{}]}).
-record(simpleContentExtension, {base :: qname(),
				 attributes :: [#attribute{}]}).
-record(group, {ref :: qname()}).
-record(choice,   {content :: [element_ish()]}).
-record(sequence, {content :: [element_ish()]}).
-record(all, {content :: [element_ish()]}).
-type(element_ish() :: element_ish() | #group{} | #choice{} | #sequence{} | #all{}).

-record(complexType, {
          content :: #simpleContentExtension{} | #complexContentRestriction{} | element_ish(), % ?
          attributes :: #attribute{}}).

-type(typedef() :: #simpleType{} | #complexType{}).

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

-type(erlsom_dom() :: {_,[_],[_]}).

-endif.
