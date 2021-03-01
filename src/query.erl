-module(query).
-include("./stockman.hrl").
-export([total_sales/1,
         most_expensive_invoice/1,
         most_expensive_product/1,
         avg_price_per_product/1,
         total_sales_per_customer/1,
         customer_with_largest_sales/1,
         customers_with_least_sales/2,
         date_with_largest_total_sales/1]).

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

total_sales_per_customer(Invoices) ->
    Totals = dict:to_list(total_sales_per_customer_dict(Invoices)),
    lists:sort(fun({_, Total1}, {_, Total2}) -> Total1 > Total2 end, Totals).

total_sales_per_customer_dict(Invoices) ->
    dict:fold(fun(_, #invoice{customer=Customer, total=InvoiceTotal}, Result) ->
                      dict:update(Customer,
                                  fun(Total) -> Total + InvoiceTotal end,
                                  InvoiceTotal,
                                  Result)
              end,
              dict:new(),
              Invoices).

customer_with_largest_sales(Invoices) ->
    case total_sales_per_customer(Invoices) of
        [{Customer,_}|_] ->
            Customer;
        _ ->
            undefined
    end.

customers_with_least_sales(N, Invoices) ->
    Totals = total_sales_per_customer(Invoices),
    RTotals = lists:reverse(Totals),
    RResult = lists:sublist(RTotals, N),
    lists:reverse(RResult).

date_with_largest_total_sales(Invoices) ->
    DateTotals = dict:fold(fun(_, #invoice{trx_ts=Ts, total=InvoiceTotal}, Result) ->
                                   Date = lists:sublist(Ts, 10),
                                   dict:update(Date,
                                               fun(DateTotal) ->
                                                       DateTotal + InvoiceTotal
                                               end,
                                               InvoiceTotal,
                                               Result)
                           end,
                           dict:new(),
                           Invoices),
    case dict:to_list(DateTotals) of
        [] ->
            undefined;
        [Elem1] ->
            Elem1;
        [Elem1|T] ->
            lists:foldl(fun({_,Total1}=Elem, {_, Total2}=Acc) ->
                                if
                                    Total1 > Total2 ->
                                        Elem;
                                    true ->
                                        Acc
                                end
                        end,
                        Elem1,
                        T)
    end.

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

total_sales_per_customer_test() ->
    [] = total_sales_per_customer(dict:new()),

    Invoices = dict:from_list(
                 [{"i1", #invoice{doc_no="i1", customer="c1", total=10.0}},
                  {"i2", #invoice{doc_no="i2", customer="c2", total=20.0}},
                  {"i3", #invoice{doc_no="i3", customer="c1", total=30.5}}]
                ),
    [{"c1", 40.5}, {"c2", 20.0}] = total_sales_per_customer(Invoices).

customer_with_largest_sales_test() ->
    undefined = customer_with_largest_sales(dict:new()),

    Invoices = dict:from_list(
                 [{"i1", #invoice{doc_no="i1", customer="c1", total=10.0}},
                  {"i2", #invoice{doc_no="i2", customer="c2", total=20.0}},
                  {"i3", #invoice{doc_no="i3", customer="c1", total=30.5}}]
                ),
    "c1" = customer_with_largest_sales(Invoices).

customers_with_least_sales_test() ->
    [] = customers_with_least_sales(2, dict:new()),

    Invoices = dict:from_list(
                 [{"i1", #invoice{doc_no="i1", customer="c1", total=10.0}},
                  {"i2", #invoice{doc_no="i2", customer="c2", total=20.0}},
                  {"i3", #invoice{doc_no="i3", customer="c1", total=30.5}}]
                ),
    [{"c1", 40.5}, {"c2", 20.0}] = customers_with_least_sales(2, Invoices),

    Invoices1 = dict:from_list(
                  [{"i1", #invoice{doc_no="i1", customer="c1", total=10.0}},
                   {"i2", #invoice{doc_no="i2", customer="c2", total=20.0}},
                   {"i3", #invoice{doc_no="i3", customer="c3", total=5.0}},
                   {"i4", #invoice{doc_no="i4", customer="c4", total=41.0}},
                   {"i5", #invoice{doc_no="i5", customer="c5", total=18.0}},
                   {"i6", #invoice{doc_no="i6", customer="c1", total=30.5}}]
                 ),
    [{"c2", 20.0}, {"c5", 18.0}, {"c3", 5.0}] = customers_with_least_sales(3, Invoices1).

date_with_largest_sales_test() ->
    undefined = date_with_largest_total_sales(dict:new()),

    Invoices = dict:from_list(
                 [{"i1", #invoice{trx_ts="d1", total=10.0}},
                  {"i2", #invoice{trx_ts="d2", total=20.0}},
                  {"i3", #invoice{trx_ts="d1", total=30.0}},
                  {"i4", #invoice{trx_ts="d3", total=5.0}},
                  {"i5", #invoice{trx_ts="d2", total=10.0}}]
                ),
    {"d1", 40.0} = date_with_largest_total_sales(Invoices).

-endif.
