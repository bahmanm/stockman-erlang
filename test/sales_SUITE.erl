-module(sales_SUITE).

-include("../src/stockman.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test__save_invoice/1]).

all() ->
    [test__save_invoice].

init_per_testcase(_, Config) ->
    {ok, InventoryPid} = inventory:start(),
    {ok, SalesPid} = sales:start(),
    [{inventory_pid, InventoryPid} | [{sales_pid, SalesPid} | Config]].

end_per_testcase(_, Config) ->
    exit(?config(inventory_pid, Config), end_per_testcase),
    exit(?config(sales_pid, Config), end_per_testcase).

test__save_invoice(_Config) ->
    inventory:move_in("p1", 10),
    inventory:move_in("p2", 10),
    Invoice = #invoice{
        type = sales,
        bpartner = "c1",
        doc_no = "SI-10",
        trx_ts = "2023-01-01",
        total = 100,
        discount = 0,
        lines = [
            #invoice_line{line_no = 10, product = "p1", qty = 5, price = 15, line_amt = 75},
            #invoice_line{line_no = 20, product = "p2", qty = 1, price = 25, line_amt = 25}
        ]
    },
    ok = sales:save_invoice(Invoice),
    {ok, 5} = inventory:available_qty("p1"),
    {ok, 9} = inventory:available_qty("p2").
