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
		  type :: #simpleType{}}).

-record(elementRef, {ref :: qname()}).

-record(attribute, {name :: ncname(),
		    type :: qname(),
		    use=optional :: optional | prohibited | required}).

-type(ncname() :: string()). %% "non-colonized" name
-type(anyURI() :: string()).
-type(qname() :: {anyURI(), ncname()}).
-record(simpleContentExtension, {base :: qname(),
				 attributes :: [#attribute{}]}).
-record(complexType, {children :: [#element{} | #elementRef{}] | #simpleContentExtension{}}).

-type(typedef() :: #simpleType{} | #complexType{}).

-record(definitions, {
          types=[] :: [typedef()],
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
