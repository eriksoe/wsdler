-ifndef(wsdler_hrl).
-define(wsdler_hrl, included).

%-type(typedef(), #simpleType{} | #complexType{}).

-record(definitions, {
          types=[]
         }).
-record(types, {
          types=[]
         }).

%% Simpletype choices:
-record(simpleRestriction,
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
-type(simpleDerivation() :: #simpleRestriction{} | #simpleListType{} | #simpleUnionType{}).

-record(simpleType, {type :: {named,_} | simpleDerivation()}).
-record(complexType, {}).
-record(element, {name :: _, type :: #simpleType{}}).

-endif.
