-module(inventory_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test__available_qty/1, test__move_out/1]).

all() ->
    [test__available_qty, test__move_out].

init_per_testcase(_, Config) ->
    {ok, InventoryPid} = inventory:start(),
    [{inventory_pid, InventoryPid} | Config].

end_per_testcase(_, Config) ->
    exit(?config(inventory_pid, Config), end_per_testcase).

test__available_qty(_Config) ->
    {error, unknown_product} = inventory:available_qty("p1"),
    ok = inventory:move_in("p1", 10),
    ok = inventory:move_in("p2", 5),
    {ok, 10} = inventory:available_qty("p1"),
    {ok, 5} = inventory:available_qty("p2").

test__move_out(_Config) ->
    {error, unknown_product} = inventory:move_out("p1", 10),
    ok = inventory:move_in("p1", 10),
    {error, insufficient_qty} = inventory:move_out("p1", 15),
    ok = inventory:move_out("p1", 8),
    {ok, 2} = inventory:available_qty("p1").
