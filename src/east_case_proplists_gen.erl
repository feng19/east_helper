-module(east_case_proplists_gen).
-include("ast_helper.hrl").

%% API
-export([
    gen_module/3,
    gen_module/4,
    gen_multi_fun_module/2,
    gen_function/2,
    gen_function/3,
    gen_function/4,
    format/1
]).

-type proplists() :: [{any() | list(), any()}, ...].
-type default_value() :: any().

%%====================================================================
%% API functions
%%====================================================================

-spec gen_module(module(), proplists(), default_value()) -> ok.
gen_module(Module, PropLists, DefaultValue) ->
    Arity = arity(PropLists),
    gen_module(Module, Arity, PropLists, DefaultValue).

-spec gen_module(module(), arity(), proplists(), default_value()) -> ok.
gen_module(Module, Arity, PropLists, DefaultValue) ->
    Forms = [
        erl_syntax:revert(?attribute(module, [?atom(Module)])),
        erl_syntax:revert(?export(all, 0)),
        erl_syntax:revert(?export(term, Arity)),
        erl_syntax:revert(?function(all, [?clause([], none, [?abstract(PropLists)])])),
        gen_function(term, Arity, PropLists, DefaultValue)
    ],
%%    io:format("~ts~n", [erl_prettypr:format(erl_syntax:form_list(Forms))]),
    parse_trans_mod:compile_and_load_forms(Forms, [verbose, report_errors]).

-spec gen_multi_fun_module(module(),
    FunList :: [
    {FunName :: atom(), arity(), proplists(), default_value()} |
    {FunName :: atom(), proplists(), default_value()}]) -> ok.
gen_multi_fun_module(Module, FunList) ->
    {ExportList, FunctionList, AllProplists} =
        lists:foldl(
            fun({FunName, Arity, PropLists, DefaultValue}, Acc) ->
                gen_multi_fun_module_do(FunName, Arity, PropLists, DefaultValue, Acc);
                ({FunName, PropLists, DefaultValue}, Acc) ->
                    Arity = arity(PropLists),
                    gen_multi_fun_module_do(FunName, Arity, PropLists, DefaultValue, Acc)
            end, {[], [], []}, FunList),

    Forms = [
        erl_syntax:revert(?attribute(module, [?atom(Module)])),
        erl_syntax:revert(?export(all, 0)),
        erl_syntax:revert(?export(all, 1))
        | ExportList] ++ [
        erl_syntax:revert(?function(all, [?clause([], none, [?abstract(AllProplists)])])),
        gen_function(all, AllProplists) | FunctionList],
%%    io:format("~ts~n", [erl_prettypr:format(erl_syntax:form_list(Forms))]),
    parse_trans_mod:compile_and_load_forms(Forms, [verbose, report_errors]).

gen_multi_fun_module_do(FunName, Arity, PropLists, DefaultValue, {ExportListAcc, FunctionListAcc, AllProplistsAcc}) ->
    {
        [erl_syntax:revert(?export(FunName, Arity)) | ExportListAcc],
        [gen_function(FunName, Arity, PropLists, DefaultValue) | FunctionListAcc],
        [{FunName, PropLists} | AllProplistsAcc]
    }.

%% @doc code gen kv list case function, if not case return default value
%% <pre>
%% io:format("~ts", [format(gen_function(test, [{1, 1}, {2, 2}], 3))]).
%%     test(1) -> 1;
%%     test(2) -> 2;
%%     test(_) -> 3.
%% io:format("~ts", [format(gen_function(test, [{[1, 1], 1}, {[2, 2], 2}], 3))]).
%%     test(1, 1) -> 1;
%%     test(2, 2) -> 2;
%%     test(_, _) -> 3.
%% </pre>
%% @end
-spec gen_function(FunctionName :: atom(), PropLists0 :: [{any() | list(), any()}, ...],
    DefaultValue :: any()) -> erl_syntax:syntaxTree().
gen_function(FunctionName, PropLists0, DefaultValue) ->
    Arity = arity(PropLists0),
    gen_function(FunctionName, Arity, PropLists0, DefaultValue).

-spec gen_function(FunctionName :: atom(), Arity :: pos_integer(), PropLists0 :: [{any() | list(), any()}, ...],
    DefaultValue :: any()) -> erl_syntax:syntaxTree().
gen_function(FunctionName, Arity, PropLists0, DefaultValue) ->
    PropLists = [T || {_, V} = T <- PropLists0, V =/= DefaultValue],
    gen_function(FunctionName, PropLists ++ [{lists:duplicate(Arity, '_'), DefaultValue}]).

%% @doc code gen kv list case function
%% <pre>
%% io:format("~ts", [format(gen_function(test, [{1, 1}, {2, 2}]))]).
%%     test(1) -> 1;
%%     test(2) -> 2.
%% io:format("~ts", [format(gen_function(test, [{[1, 1], 1}, {[2, 2], 2}]))]).
%%     test(1, 1) -> 1;
%%     test(2, 2) -> 2.
%% </pre>
%% @end
-spec gen_function(FunctionName :: atom(), PropLists0 :: [{any() | list(), any()}, ...]) ->
    erl_syntax:syntaxTree().
gen_function(FunctionName, PropLists) ->
    Clauses =
        [begin
             Patterns = trans_args(Args),
             ?clause(Patterns, none, [?abstract(V)])
         end || {Args, V} <- PropLists],
    SyntaxTree = ?function(FunctionName, Clauses),
    erl_syntax:revert(SyntaxTree).

-spec format(Forms :: erl_syntax:syntaxTree()) -> string().
format(Forms) ->
    erl_prettypr:format(Forms).

%%====================================================================
%% Internal functions
%%====================================================================

arity(PropLists) ->
    case element(1, hd(PropLists)) of
        Args when is_list(Args) ->
            length(Args);
        _ -> 1
    end.

trans_args(Args) when is_list(Args) ->
    [begin
         case Arg of
             '_' ->
                 ?var('_');
             _ ->
                 ?abstract(Arg)
         end
     end || Arg <- Args];
trans_args('_') -> [?var('_')];
trans_args(Arg) -> [?abstract(Arg)].
