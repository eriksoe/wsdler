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

record_types_test() ->
    {{record,[]}, {record_cons,[]}} =
        wsdler_expr:type_check({record_cons, []}, []),

    RecordConsExp = {record_cons,
                     [{field_1, {boolean_literal, true}},
                      {field_2, {integer_literal, 123}},
                      {field_z, {string_literal, "xyzzy"}}
                     ]},
    RecordConsDecoratedExp =
        {{record,[{field_1,boolean},
                  {field_2,integer},
                  {field_z,string}]},
         {record_cons,
          [{field_1, {boolean,{literal, true}}},
           {field_2, {integer,{literal, 123}}},
           {field_z, {string,{literal, "xyzzy"}}}
          ]}},
    true = RecordConsDecoratedExp =:= wsdler_expr:type_check(RecordConsExp, []),

    {boolean, {record_lookup, field_1, RecordConsDecoratedExp}} =
        wsdler_expr:type_check({record_lookup, field_1, RecordConsExp}, []),

    ok.
