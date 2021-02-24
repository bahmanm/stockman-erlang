-module(stockman).
-include("./stockman.hrl").
-export([main/1]).

main([Filepath]) ->
    main([Filepath, "v1"]);

main([Filepath,Version]) ->
    Invoices = loader:load_file(Filepath),
    case Version of
        "v1" ->
            v1(Invoices);
        "v2" ->
            v2(Invoices);
        _ ->
            io:fwrite("Invalid version")
    end,
    erlang:halt(0).

v1(Invoices) ->
    printer:pprint(Invoices).

v2(Invoices) ->
    Separator = list_to_binary(lists:duplicate(20, "%")),
    case dict:is_empty(Invoices) of
        true ->
            ok;
        _ ->
            io:fwrite("total sales: ~.2f~n",
                      [query:total_sales(Invoices)]),
            io:fwrite("~s~n", [Separator]),
            %%
            printer:pprint(query:most_expensive_invoice(Invoices)),
            io:fwrite("~s~n", [Separator]),
            %%
            {MostExpensiveProduct, _} = query:most_expensive_product(Invoices),
            io:fwrite("most expensive product: ~s~n",
                      [MostExpensiveProduct]),
            io:fwrite("~s~n", [Separator]),
            %%
            io:fwrite("average price per product~n", []),
            lists:foreach(fun({P,A}) ->
                                  io:fwrite("~s, ~.2f~n", [P, A])
                          end,
                          query:avg_price_per_product(Invoices)),
            io:fwrite("~s~n", [Separator]),
            %%
            io:fwrite("total sales per customer~n", []),
            lists:foreach(fun({C,S}) ->
                                  io:fwrite("~s, ~.2f~n", [C, S])
                          end,
                          query:total_sales_per_customer(Invoices)),
            io:fwrite("~s~n", [Separator]),
            %%
            io:fwrite("customer with largest total sales: ~s~n",
                      [query:customer_with_largest_sales(Invoices)]),
            io:fwrite("~s~n", [Separator]),
            %%
            io:fwrite("3 customer with least total sales: ~n", []),
            lists:foreach(fun({C,_}) ->
                                  io:fwrite("~s~n", [C])
                          end,
                          query:customers_with_least_sales(3, Invoices)),
            io:fwrite("~s~n", [Separator]),
            %%
            {Date, Total} = query:date_with_largest_total_sales(Invoices),
            io:fwrite("date with largest total sales: ~s, ~.2f~n",
                      [Date, Total]),
            io:fwrite("~s~n", [Separator])
    end.
