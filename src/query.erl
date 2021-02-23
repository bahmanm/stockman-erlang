-module(query).
-include("./stockman.hrl").
-export([total_sales/1]).

total_sales(Invoices) ->
    dict:fold(
      fun(_, #invoice{total=Total}, TotalSales) ->
              TotalSales + Total
      end,
      0.0,
      Invoices
     ).



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

-endif.
