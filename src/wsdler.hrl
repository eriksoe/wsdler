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

-record(elementRef, {ref :: qname()}).
-type(element() :: #element{} | #elementRef{}).
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
-type(element_alike() :: term()). %% element, choice, sequence, ...
-record(group, {ref :: qname()}).
-record(choice,   {content :: [element_alike()]}).
-record(sequence, {content :: [element_alike()]}).
-record(all, {content :: [element_alike()]}).

-record(complexType, {content :: #simpleContentExtension{} | #complexContentRestriction{} | element_alike(),
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
