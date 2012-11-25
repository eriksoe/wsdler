-module(wsdler_expr).

-export([type_check/2, evaluate/2]).

-compile(export_all). % For now.

-type(equality_type() ::
      boolean | integer | string
      | {record, [{atom(), equality_type()}]}).

-type(type() ::
      boolean | integer | string
      | {record, [{atom(), type()}]}
      | {list, type()}
      | {set, equality_type()}
      | {map, [{equality_type(), type()}]}
      | {incomplete, type()}
     ).

%%%========== Type checker / decorator ========================================

%% Literals:
type_check({boolean_literal, _}, _Env) -> boolean;
type_check({integer_literal, _}, _Env) -> integer;
type_check({float_literal, _},   _Env) -> float;
type_check({time_literal, _},    _Env) -> time;
type_check({string_literal, _},  _Env) -> string;
%% Boolean ops:
type_check({'and', _, _}=E0, Env) -> type_check_bool_binop(E0, Env);
type_check({'or',  _, _}=E0, Env) -> type_check_bool_binop(E0, Env);
type_check({'not', _   }=E0, Env) -> type_check_bool_unop(E0, Env);

%% Number ops:

%% Record ops:
type_check({record_cons, FVs}, Env) ->
    {record, [{F,type_check(V,Env)} || {F,V} <- FVs]};
type_check({record_lookup, F, Exp}=E0, Env) ->
    {record, FTs} = RT = check_record_type(Exp,Env,E0),
    case lists:lookup(F,1,FTs) of
        {_,FT} -> FT;
        false -> type_error({record_access, F, RT}, E0)
    end.

type_check_bool_unop({_Unop, A}=E0, Env) ->
    incomplete_if_any_is([type_check(A,Env)],
                         fun ([AT]) ->
                                 if AT==boolean -> boolean;
                                    AT/=boolean -> type_error({operand_not_boolean, AT}, E0)
                                 end
                         end).

type_check_bool_binop({_Binop, A, B}=E0, Env) ->
    incomplete_if_any_is([type_check(A,Env), type_check(B,Env)],
                         fun ([AT,BT]) ->
                                 if AT==boolean, BT==boolean ->
                                         boolean;
                                    AT/=boolean ->
                                         type_error({left_operand_not_boolean, AT}, E0);
                                    BT/=boolean ->
                                         type_error({right_operand_not_boolean, AT}, E0)
                                 end
                         end).

incomplete_if_any_is(L, Fun) ->
    incomplete_if_any_is(L,Fun,[],false).
incomplete_if_any_is([], Fun, Acc, AnyIncomplete) ->
    Type = Fun(lists:reverse(Acc)),
    if AnyIncomplete -> {incomplete, Type};
       true -> Type
    end;
incomplete_if_any_is([H|T], Fun, Acc, AnyIncomplete) ->
    case H of
        {incomplete, HType} ->
            AnyIncomplete2 = true;
        HType ->
            AnyIncomplete2 = AnyIncomplete
    end,
    incomplete_if_any_is(T, Fun, [HType|Acc], AnyIncomplete2).


check_record_type(Exp,Env,E0) ->
    case type_check(Exp,Env) of
        {record,_}=T -> T;
        T-> type_error({not_a_record_type, T}, E0)
    end.

type_error(Reason, Exp) ->
    error({type_error, Reason, Exp}).


%%%========== Evaluator ========================================
-define(unknown_check(Par,Var,Body),
        case Par of unknown -> unknown; Var -> Body end).

evaluate({boolean_literal, V}, _Env) -> V;
evaluate({integer_literal, V}, _Env) -> V;
evaluate({float_literal, V},   _Env) -> V;
evaluate({time_literal, V},    _Env) -> V;
evaluate({string_literal, V},  _Env) -> V;
%% Boolean ops:
evaluate({'and', A, B}, Env) ->
    case evaluate(A,Env) of
        {boolean, true} -> evaluate(B,Env);
        Res -> Res                              % false or unknown
    end;
evaluate({'or', A, B}, Env) ->
    case evaluate(A,Env) of
        {boolean, false} -> evaluate(B,Env);
        Res -> Res                              % true or unknown
    end;
evaluate({'not'=Op, A}, Env) -> evaluate_unop(Op, A, Env).

%%%==========

evaluate_unop(Op, E1, Env) ->
    V1 = evaluate(E1, Env),
    perform_unop(Op, V1).

evaluate_binop(Op, E1, E2, Env) ->
    V1 = evaluate(E1, Env),
    V2 = evaluate(E2, Env),
    perform_binop(Op, V1, V2).

%%%==========

perform_unop(_, unknown) -> unknown;
perform_unop('not', V) -> not V.

perform_binop(_, unknown, _) -> unknown;
perform_binop(_, _, unknown) -> unknown;
perform_binop('int_+', V1, V2) ->
    if is_integer(V1), is_integer(V2) -> V1+V2;
       true ->
            {in_range, V1min, V1max} = rangeify(V1),
            {in_range, V2min, V2max} = rangeify(V2),
            Min = ?unknown_check(V1min,A, ?unknown_check(V2min, B, A+B)),
            Max = ?unknown_check(V1max,C, ?unknown_check(V2max, D, C+D)),
            {in_range, Min,Max}
    end;
perform_binop('int_-', V1, V2) ->
    if is_integer(V1), is_integer(V2) -> V1-V2;
       true ->
            {in_range, V1min, V1max} = rangeify(V1),
            {in_range, V2min, V2max} = rangeify(V2),
            Min = ?unknown_check(V1min,A, ?unknown_check(V2max, D, A-D)),
            Max = ?unknown_check(V1max,C, ?unknown_check(V2min, B, C-B)),
            {in_range, Min,Max}
    end.


rangeify({in_range,_,_}=Range) -> Range;
rangeify(V) -> {in_range, V, V}.
