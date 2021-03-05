-module(stockman).
-include("./stockman.hrl").
-export([main/1]).

main(#{mode := "v1", sales_invoices := Filepath}) ->
    #{invoices := Invoices} = loader:load_file(invoice, Filepath),
    v1(Invoices),
    erlang:halt(0);

main(#{mode := "v2", sales_invoices := Filepath}) ->
    #{invoices := Invoices} = loader:load_file(invoice, Filepath),
    v2(Invoices),
    erlang:halt(0);

main(#{mode := "v3", sales_invoices := InvoicesFilepath,
       inventories := InventoriesFilepath})
->
    Inventories = loader:load_file(inventory, InventoriesFilepath),
    InvoicesResult = loader:load_file(invoice, InvoicesFilepath),
    v3(Inventories, InvoicesResult),
    erlang:halt(0);

main(#{mode := "v4", sales_invoices := InvoicesFilepath,
       inventories := InventoriesFilepath})
->
    Inventories = loader:load_file(inventory, InventoriesFilepath),
    InvoicesResult = loader:load_file(invoice, InvoicesFilepath),
    v4(Inventories, InvoicesResult),
    erlang:halt(0);

main(Args) ->
    OptSpec =
        [{mode, undefined, undefined, {string, "v4"},
          "mode in which stockman operates - v1|v2|v3|v4"},
         {sales_invoices, undefined, "sales-invoices", string,
          "path to sales invoices csv file"},
         {inventories, undefined, "inventories", string,
          "path to inventories csv file"}],
    case getopt:parse(OptSpec, Args) of
        {ok, {OptionsTList, _}} ->
            case maps:from_list(OptionsTList) of
                #{mode := _, sales_invoices := _, inventories := _}=Options ->
                    main(Options);
                _ ->
                    io:format("~s", [getopt:usage(OptSpec, "stockman")])
            end;
        {error, Details} ->
            io:format("~s~n~p~n", [getopt:usage(OptSpec, "stockman"),
                                   Details])
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

v3(Inventories, #{invoices := InvoicesToImport, load_order := Order}) ->
    {_, _, Errors} =
        sales:save_invoices(
          #{to_save => InvoicesToImport,
            order => Order,
            inventories => Inventories,
            saved => dict:new()}),
    lists:foreach(fun(#{invoice := Invoice, line_no := LineNo}) ->
                          io:format("Line No: ~B~n", [LineNo]),
                          printer:pprint(Invoice)
                  end,
                  Errors).

v4(Inventories, #{invoices := InvoicesToImport}) ->
    {_, _, Errors} =
        sales:save_invoices(
          #{to_save => InvoicesToImport,
            order => ascending_timestamp,
            inventories => Inventories,
            saved => dict:new()}),
    lists:foreach(fun(#{invoice := Invoice, line_no := LineNo}) ->
                          io:format("Line No: ~B~n", [LineNo]),
                          printer:pprint(Invoice)
                  end,
                  Errors).
