-module(east_helper).
-include("ast_helper.hrl").

%% API exports
-export([
    make_binary_tree/1,
    format_function_value/1, format_function_value_tree/1,
    format_function_value_new_line/1, format_function_value_new_line_tree/1
]).

%%====================================================================
%% API functions
%%====================================================================

make_binary_tree(UnicodeList) ->
    erl_syntax:binary([erl_syntax:binary_field(?string(UnicodeList), [?abstract('utf8')])]).

format_function_value(Value) ->
    format_function_value_tree(?abstract(Value)).
format_function_value_tree(SyntaxTree) ->
    IoList = erl_prettypr:format(SyntaxTree, [{paper, 100}, {ribbon, 80}]),
    unicode:characters_to_binary(IoList).

format_function_value_new_line(Value) ->
    format_function_value_new_line_tree(?abstract(Value)).
format_function_value_new_line_tree(SyntaxTree) ->
    Forms = ?function(test, [?clause([], none, [SyntaxTree])]),
    IoList0 = erl_prettypr:format(Forms, [{paper, 100}, {ribbon, 80}]),
    %% and del last '.'
    IoList1 = lists:reverse(tl(lists:reverse(IoList0))),
    %% del 'test() ->'
    IoList2 = lists:nthtail(9, IoList1),
    case IoList2 of
        [10 | IoList3] -> % \n
            IoList = lists:nthtail(4, IoList3),
            unicode:characters_to_binary(IoList);
        [32 | IoList3] -> % Space
            unicode:characters_to_binary(IoList3)
    end.

%%====================================================================
%% Internal functions
%%====================================================================
