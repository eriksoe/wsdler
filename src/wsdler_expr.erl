-module(wsdler_expr).

-export([type_check/2, evaluate/2]).

-compile(export_all). % For now.

%%% Purpose: Type checking and evaluation of expressions.
%%%
%%% The focus is not on performance, but on features -- in particular,
%%% intuitive handling of unknown and partially unknown values --
%%% as well as code clarity.
%%% For this reason, there is in fact an "interpreter within the interpreter"
%%% [see binop_spec and exec_binop_spec] for handling operators in a
%%% uniform and concise manner.

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
type_check({boolean_literal, V}, _Env) when is_boolean(V) ->
    {boolean, {literal,V}};
type_check({integer_literal, V}, _Env) when is_integer(V) ->
    {integer, {literal,V}};
type_check({float_literal, V},   _Env) when is_number(V) ->
    {float,   {literal,V}};
type_check({time_literal, V},    _Env) ->
    % TODO: Check the value.
    {time,    {literal,V}};
type_check({string_literal, V},  _Env) when is_list(V) ->
    % TODO: Check the value.
    {string,  {literal,V}};
%% TODO: Change the rest of type_check to return {Type, InternalExp}.
%% TODO: Make handling of operators data-driven.
%% Boolean ops:
type_check({'and', _, _}=E0, Env) -> type_check_binop(E0, Env);
type_check({'or',  _, _}=E0, Env) -> type_check_binop(E0, Env);
type_check({'not', _   }=E0, Env) -> type_check_unop(E0, Env);

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

type_check_unop({Op, A}, Env) ->
    CheckedOperands = [type_check(X,Env) || X <- [A]],
    OperandTypes = [Type || {Type,_} <- CheckedOperands],
    {ConcreteOp, Type} = type_check_function(Op, OperandTypes),
    [CkdA] = CheckedOperands,
    {Type, {unop, ConcreteOp, CkdA}}.

type_check_binop({Op, A, B}, Env) ->
    CheckedOperands = [type_check(X,Env) || X <- [A,B]],
    OperandTypes = [Type || {Type,_} <- CheckedOperands],
    {ConcreteOp, Type} = type_check_function(Op, OperandTypes),
    [CkdA,CkdB] = CheckedOperands,
    {Type, {binop, ConcreteOp, CkdA, CkdB}}.

%%%========== Function types: =============================================
%%%==== Boolean operators:
type_check_function('not', [boolean]) -> {'not',boolean};
type_check_function('not', [{incomplete,boolean}]) -> {'i_not',{incomplete, boolean}};
type_check_function('and', [boolean,boolean]) -> {'and',boolean};
type_check_function('and', [{incomplete,boolean},{incomplete,boolean}]) -> {'i_and',{incomplete, boolean}};
type_check_function('or', [boolean,boolean]) -> {'or',boolean};
type_check_function('or', [{incomplete,boolean},{incomplete,boolean}]) -> {'i_or',{incomplete, boolean}};
type_check_function(Function, OperandTypes) ->
    error({type_error, {bad_function_operand_types, Function, OperandTypes}}).


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

evaluate({literal, V}, _Env) -> V;
%% Binary operators:
evaluate({binop, Op, AExp, BExp}, Env) ->
    AVal = evaluate(AExp, Env),
    BVal = evaluate(BExp, Env),

    Spec = binop_spec(Op),
    exec_binop_spec(Spec, AVal, BVal).

exec_binop_spec([{annihilator, V} | Spec], A, B) ->
    if A=:=V;
       B=:=V ->
            V;
       true ->
            exec_binop_spec(Spec, A, B)
    end;
exec_binop_spec([{fully_known, Fun} | Spec], A, B) when is_function(Fun,2) ->
    BothKnown = is_known(A) andalso is_known(B),
    if BothKnown ->
            Fun(A,B);
       true ->
            exec_binop_spec(Spec, A, B)
    end;
exec_binop_spec([{monotone, DirA, DirB} | Spec], A, B) ->
    if element(1,A)==in_range;
       element(1,B)==in_range ->
            handle_monotone(polarize(rangeify(A),DirA),
                            polarize(rangeify(B),DirB), Spec);
       true ->
            exec_binop_spec(Spec, A, B)
    end.

polarize(Range, up) -> Range;
polarize({in_range, Min,Max}, down) -> {in_range, Max, Min}.

handle_monotone({in_range, A1, A2}, {in_range, B1, B2}, Spec) ->
    {in_range,
     exec_binop_spec(Spec, A1, B1),
     exec_binop_spec(Spec, A2, B2)}.

binop_spec('and') ->
    [{annihilator, false}, {annihilator, unknown},
     {fully_known, fun(A,B)->A and B end}];
binop_spec('or') ->
    [{annihilator, true}, {annihilator, unknown},
     {fully_known, fun(A,B)->A or B end}];
binop_spec('+') ->
    [{annihilator, unknown},
     {monotone, up, up},
     {fully_known, fun(A,B)->A+B end}];
binop_spec('-') ->
    [{annihilator, unknown},
     {monotone, up, down},
     {fully_known, fun(A,B)->A-B end}];
binop_spec('min') ->
    [{annihilator, unknown},
     {monotone, up, up},
     {fully_known, fun(A,B)->min(A,B) end}];
binop_spec('max') ->
    [{annihilator, unknown},
     {monotone, up, up},
     {fully_known, fun(A,B)->max(A,B) end}].

rangeify({in_range,_,_}=Range) -> Range;
rangeify(V) -> {in_range, V, V}.

is_known(unknown) -> false;
is_known({in_range, _, _}) -> false;
is_known(_) -> true.

