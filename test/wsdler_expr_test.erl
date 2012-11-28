-module(wsdler_expr_test).
-compile(export_all).

literal_types_test() ->
    {boolean, {literal,true}} =
        wsdler_expr:type_check({boolean_literal, true}, []),

    {integer, {literal,123}} =
        wsdler_expr:type_check({integer_literal, 123}, []),

    {string, {literal,"abc123XYZ"}} =
        wsdler_expr:type_check({string_literal, "abc123XYZ"}, []),
    ok.

boolean_op_types_test() ->
    {boolean, {unop, 'not', {boolean,{literal, false}}}} =
        wsdler_expr:type_check({'not', {boolean_literal, false}}, []),

    {boolean, {binop, 'and',
               {boolean,{literal, false}},
               {boolean, {literal, true}}}} =
        wsdler_expr:type_check({'and',
                                {boolean_literal, false},
                                {boolean_literal, true}}, []),

    {boolean, {binop, 'or',
               {boolean,{literal, false}},
               {boolean, {literal, true}}}} =
        wsdler_expr:type_check({'or',
                                {boolean_literal, false},
                                {boolean_literal, true}}, []),
    ok.
