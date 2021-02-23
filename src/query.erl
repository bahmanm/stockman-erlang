-module(query).
-include("./stockman.hrl").
-export([total_sales/1,
         most_expensive_invoice/1,
         avg_price_per_product/1]).

total_sales(Invoices) ->
    dict:fold(
      fun total_sales/3,
      undefined,
      Invoices
     ).

total_sales(_, #invoice{total=Total}, undefined) ->
    Total;

total_sales(_, #invoice{total=Total}, TotalSales) ->
    TotalSales + Total.

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

most_expensive_product(Invoices) ->
    dict:fold(fun most_expensive_product/3, undefined, Invoices).

most_expensive_product(_,
                       #invoice{lines=Lines},
                       Result
                      ) ->
    most_expensive_product(Lines, Result).

most_expensive_product([#invoice_line{product=Product, price=Price}|Lines],
                       {_, ResultPrice}=Result
                      ) ->
    CurrentResult = if
                        Price > ResultPrice ->
                            {Product, Price};
                        true ->
                            Result
                    end,
    most_expensive_product(Lines, CurrentResult);

most_expensive_product([#invoice_line{product=Product, price=Price}|Lines],
                       undefined) ->
    most_expensive_product(Lines, {Product, Price});

most_expensive_product([],
                       Result) ->
    Result.


avg_price_per_product(Invoices) ->
    QtyAndTotalDict = dict:fold(
                          fun avg_price_per_product/3,
                          dict:new(),
                          Invoices
                         ),
    AvgDict = dict:map(fun(_, {Qty, TotalQtyxPrice}) ->
                               TotalQtyxPrice / Qty
                       end,
                       QtyAndTotalDict),
    AvgList = dict:to_list(AvgDict),
    SortedAvgList = lists:sort(fun({_,AvgA},{_,AvgB}) ->
                                       AvgA > AvgB
                               end,
                               AvgList),
    SortedAvgList.



avg_price_per_product(_, #invoice{lines=Lines}, Result) ->
    avg_price_per_product(Lines, Result).

avg_price_per_product([#invoice_line{product=Product, price=Price, qty=Qty}|Lines],
                      Result
                     ) ->
    CurrentResult = dict:update(Product,
                                fun({RunningQty,RunningQtyxPrice}) ->
                                        {Qty+RunningQty,
                                         RunningQtyxPrice+(Qty*Price)}
                                end,
                                {Qty,Qty*Price},
                                Result),
    avg_price_per_product(Lines, CurrentResult);

avg_price_per_product([], Result) ->
    Result.



%%%
%%% tests
%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

total_sales_test() ->
    undefined = total_sales(dict:new()),

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

most_expensive_product_test() ->
    undefined = most_expensive_product(dict:new()),

    Invoices = dict:from_list(
                 [{"i1", #invoice{lines=[#invoice_line{product="P1", price=10.0},
                                         #invoice_line{product="P2", price=8.5}]}},
                  {"i2", #invoice{lines=[#invoice_line{product="P3", price=1.0},
                                         #invoice_line{product="P4", price=18.5}]}}]
                ),
    {"P4", 18.5} = most_expensive_product(Invoices).

avg_price_per_product_test() ->
    [] = avg_price_per_product(dict:new()),

    Invoices = dict:from_list(
                 [{"i1",
                   #invoice{lines=[#invoice_line{product="P1", price=10.0, qty=3},
                                   #invoice_line{product="P2", price=10.0, qty=5}]}},
                  {"i2",
                   #invoice{lines=[#invoice_line{product="P2", price=12.0, qty=3},
                                   #invoice_line{product="P3", price=18.5, qty=1}]}}]),
    [{"P3", 18.5}, {"P2", 10.75}, {"P1", 10.0}] = avg_price_per_product(Invoices).

-endif.
