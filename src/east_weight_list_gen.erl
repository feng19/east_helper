-module(east_weight_list_gen).
-include("ast_helper.hrl").

%% API
-export([
    gen_function/2,
    gen_function/3,
    gen_function/5
]).

-type function_name() :: atom().
-type operator() :: atom() | string().
-type result() :: any() | {syntaxTree, erl_syntax:syntaxTree()}.
-type weight_list() :: [{result(), Weight :: non_neg_integer()}].
-type arg() :: any() | {syntaxTree, erl_syntax:syntaxTree()}.
-type args() :: [arg()].
-type args_weight_list() :: [{args(), weight_list()}].

%%====================================================================
%% API functions
%%====================================================================

%% @doc code gen weight list string
%% example 1:
%% ``` e.g. gen_function(test, '=<', [{[], [{1, 50}, {2, 30}, {3, 20}]}]) =>
%% "test() ->
%%    case rand:uniform(100) of
%%      Random when Random =< 50 -> 1;
%%      Random when Random =< 80 -> 2;
%%      _ -> 3
%%    end." '''
%% example 2:
%% ``` e.g. gen_function(test, '=<', [{[a], [{1, 50}, {2, 30}, {3, 20}]}, {[b], [{1, 30}, {2, 30}, {3, 40}]}]) =>
%% "test(a) ->
%%    case rand:uniform(100) of
%%      Random when Random =< 50 -> 1;
%%      Random when Random =< 80 -> 2;
%%      _ -> 3
%%    end;
%%  test(b) ->
%%    case rand:uniform(100) of
%%      Random when Random =< 30 -> 1;
%%      Random when Random =< 60 -> 2;
%%      _ -> 3
%%    end." '''
%% @end

-spec gen_function(function_name(), args_weight_list()) -> string().
gen_function(FunctionName, List) ->
    gen_function(FunctionName, '=<', List).
-spec gen_function(function_name(), operator(), args_weight_list()) -> string().
gen_function(FunctionName, Op, List) ->
    gen_function(FunctionName, Op, List, rand, uniform).
gen_function(FunctionName, Op, List, RandMod, RandFun) when is_atom(FunctionName) andalso is_atom(Op) ->
    Clauses = [gen_function_do(Op, Args, WeightList, RandMod, RandFun) || {Args, WeightList} <- List],
    SyntaxTree = ?function(FunctionName, Clauses),
    Forms = erl_syntax:revert(SyntaxTree),
    erl_prettypr:format(Forms).

%%====================================================================
%% Internal functions
%%====================================================================

gen_function_do(Op, Args, WeightList, RandMod, RandFun) ->
    Patterns =
        lists:map(
            fun({syntaxTree, SyntaxTree}) ->
                SyntaxTree;
                (Arg) ->
                    ?abstract(Arg)
            end, Args),
    Body = function_body(WeightList, Op, RandMod, RandFun),
    ?clause(Patterns, none, Body).

function_body([{Result, _Weight}], _Op, _RandMod, _RandFun) ->
    [?abstract(Result)];
function_body(WeightList, Op, RandMod, RandFun) ->
    {Max, Clauses} = clauses_forms(WeightList, Op),
    case Max of
        _ when Max =< 1 ->
            error({must_gt_then_one, Max});
        _ -> ok
    end,
    [?cases(?apply(RandMod, RandFun, [?abstract(Max)]), Clauses)].

clauses_forms(WeightList, Op) ->
    clauses_forms_do(WeightList, Op, 0, []).
clauses_forms_do([{_Result, 0} | WeightList], Op, LastWeight, Clauses) -> % skip 0
    clauses_forms_do(WeightList, Op, LastWeight, Clauses);
clauses_forms_do([{Result, Weight0}], Op, LastWeight, Clauses) ->
    Weight = LastWeight + Weight0,
    Clause =
        case Result of
            {syntaxTree, SyntaxTree} ->
                ?clause([?var('_')], none, [SyntaxTree]);
            _ ->
                ?clause([?var('_')], none, [?abstract(Result)])
        end,
    clauses_forms_do([], Op, Weight, [Clause | Clauses]);
clauses_forms_do([{Result, Weight0} | WeightList], Op, LastWeight, Clauses) ->
    Weight = LastWeight + Weight0,
    Clause = clause_forms(Op, Weight, Result),
    clauses_forms_do(WeightList, Op, Weight, [Clause | Clauses]);
clauses_forms_do([], _Op, LastWeight, Clauses) ->
    {LastWeight, lists:reverse(Clauses)}.

clause_forms(Op, Weight, {syntaxTree, SyntaxTree}) ->
    Guard = ?infix(?var('Random'), Op, ?abstract(Weight)),
    ?clause([?var('Random')], Guard, [SyntaxTree]);
clause_forms(Op, Weight, Result) ->
    Guard = ?infix(?var('Random'), Op, ?abstract(Weight)),
    ?clause([?var('Random')], Guard, [?abstract(Result)]).
