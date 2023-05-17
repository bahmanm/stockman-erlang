-module(purchase_SUITE).

-include("../src/stockman.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_save_invoice/1]).

all() ->
    [test_save_invoice].

init_per_testcase(_, Config) ->
    {ok, InventoryPid} = inventory:start(),
    {ok, InvoicePid} = invoice:start(),
    [{inventory_pid, InventoryPid}, {invoice_pid, InvoicePid} | Config].

end_per_testcase(_, Config) ->
    exit(?config(inventory_pid, Config), end_per_testcase),
    exit(?config(invoice_pid, Config), end_per_testcase).

test_save_invoice(_Config) ->
    inventory:move_in("p1", 10),
    inventory:move_in("p2", 10),
    Invoice = #invoice{
        type = purchase,
        bpartner = "v1",
        doc_no = "PI-10",
        trx_ts = "2023-01-01",
        total = 100,
        discount = 0,
        lines = [
            #invoice_line{line_no = 10, product = "p1", qty = 5, price = 15, line_amt = 75},
            #invoice_line{line_no = 20, product = "p2", qty = 1, price = 25, line_amt = 25}
        ]
    },
    ok = purchase:save_invoice(Invoice),
    {ok, 15} = inventory:available_qty("p1"),
    {ok, 11} = inventory:available_qty("p2").
