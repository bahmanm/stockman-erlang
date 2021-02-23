-module(query).
-include("./stockman.hrl").
-export([total_sales/1, most_expensive_invoice/1]).

total_sales(Invoices) ->
    dict:fold(
      fun(_, #invoice{total=Total}, TotalSales) ->
              TotalSales + Total
      end,
      0.0,
      Invoices
     ).

most_expensive_invoice(Invoices) ->
    dict:fold(fun most_expensive_invoice/3, undefined, Invoices).

most_expensive_invoice(_, Invoice, undefined) ->
    Invoice;

most_expensive_invoice(_,
                       #invoice{total=Total}=Invoice,
                       #invoice{total=ResultTotal}=Result
                      ) ->
    if
        Total > ResultTotal ->
            Invoice;
        true ->
            Result
    end.

%%%
%%% tests
%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

total_sales_test() ->
    0.0 = total_sales(dict:new()),

    Invoices = dict:from_list(
                 [{"i1", #invoice{doc_no="i1", total=10.0}},
                  {"i2", #invoice{doc_no="i2", total=20.0}},
                  {"i3", #invoice{doc_no="i3", total=30.5}}]
                ),
    60.5 = total_sales(Invoices).

most_expensive_invoice_test() ->
    undefined = most_expensive_invoice(dict:new()),

    Invoices = dict:from_list(
                 [{"i1", #invoice{doc_no="i1", total=10.0}},
                  {"i2", #invoice{doc_no="i2", total=20.0}},
                  {"i3", #invoice{doc_no="i3", total=30.5}}]
                ),
    #invoice{total=30.5} = most_expensive_invoice(Invoices).

-endif.
