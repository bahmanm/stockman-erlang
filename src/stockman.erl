-module(stockman).
-include("./stockman.hrl").
-export([main/1]).

%%--------------------------------------------------------------------------------------------------
%% api
%%--------------------------------------------------------------------------------------------------

%%---
%%
%%---
main(#{mode := "v1", sales_invoices := Filepath}) ->
    #{invoices := Invoices} =
        loader:load_invoices_file(Filepath),
    v1(Invoices),
    ok;
main(#{mode := "v2", sales_invoices := Filepath}) ->
    #{invoices := Invoices} =
        loader:load_invoices_file(Filepath),
    v2(Invoices),
    ok;
main(#{
    mode := "v3",
    sales_invoices := InvoicesFilepath,
    inventories := InventoriesFilepath
}) ->
    Inventories =
        loader:load_inventories_file(InventoriesFilepath),
    InvoicesResult =
        loader:load_invoices_file(InvoicesFilepath),
    v3(Inventories, InvoicesResult),
    ok;
main(#{
    mode := "v4",
    sales_invoices := InvoicesFilepath,
    inventories := InventoriesFilepath
}) ->
    Inventories =
        loader:load_inventories_file(InventoriesFilepath),
    InvoicesResult =
        loader:load_invoices_file(InvoicesFilepath),
    v4(Inventories, InvoicesResult),
    ok;
main(#{
    mode := "v5",
    sales_invoices := SInvoicesFilepath,
    purchase_invoices := PInvoicesFilepath,
    inventories := InventoriesFilepath
}) ->
    Inventories =
        loader:load_inventories_file(InventoriesFilepath),
    SInvoicesResult =
        loader:load_invoices_file(SInvoicesFilepath),
    PInvoicesResult =
        loader:load_invoices_file(PInvoicesFilepath),
    v5(Inventories, SInvoicesResult, PInvoicesResult),
    ok;
main(InvalidOptions) when is_map(InvalidOptions) ->
    invalid_options;
main(Args) when is_list(Args) ->
    OptSpec =
        [
            {mode, undefined, undefined, {string, "v4"},
                "mode in which stockman operates - v1|v2|v3|v4|v5"},
            {sales_invoices, undefined, "sales-invoices", string,
                "path to sales invoices csv file"},
            {purchase_invoices, undefined, "purchase-invoices", string,
                "path to purchase invoices csv file"},
            {inventories, undefined, "inventories", string, "path to inventories csv file"}
        ],
    case getopt:parse(OptSpec, Args) of
        {ok, {OptionsTList, _}} ->
            Options = maps:from_list(OptionsTList),
            case main(Options) of
                invalid_options ->
                    io:format(
                        "~s~n",
                        [getopt:usage(OptSpec, "stockman")]
                    );
                _ ->
                    void
            end;
        {error, Details} ->
            io:format(
                "~s~n~p~n",
                [getopt:usage(OptSpec, "stockman"), Details]
            )
    end,
    erlang:halt(0).

%%--------------------------------------------------------------------------------------------------
%% private
%%--------------------------------------------------------------------------------------------------

%%---
%%
%%---
v1(Invoices) ->
    printer:pprint(Invoices).

%%---
%%
%%---
v2(Invoices) ->
    Separator = list_to_binary(lists:duplicate(20, "%")),
    case dict:is_empty(Invoices) of
        true ->
            ok;
        _ ->
            io:fwrite(
                "total sales: ~.2f~n",
                [query:total_sales(Invoices)]
            ),
            io:fwrite("~s~n", [Separator]),
            %%
            printer:pprint(query:most_expensive_invoice(Invoices)),
            io:fwrite("~s~n", [Separator]),
            %%
            {MostExpensiveProduct, _} = query:most_expensive_product(Invoices),
            io:fwrite(
                "most expensive product: ~s~n",
                [MostExpensiveProduct]
            ),
            io:fwrite("~s~n", [Separator]),
            %%
            io:fwrite("average price per product~n", []),
            lists:foreach(
                fun({P, A}) ->
                    io:fwrite("~s, ~.2f~n", [P, A])
                end,
                query:avg_price_per_product(Invoices)
            ),
            io:fwrite("~s~n", [Separator]),
            %%
            io:fwrite("total sales per customer~n", []),
            lists:foreach(
                fun({C, S}) ->
                    io:fwrite("~s, ~.2f~n", [C, S])
                end,
                query:total_sales_per_customer(Invoices)
            ),
            io:fwrite("~s~n", [Separator]),
            %%
            io:fwrite(
                "customer with largest total sales: ~s~n",
                [query:customer_with_largest_sales(Invoices)]
            ),
            io:fwrite("~s~n", [Separator]),
            %%
            io:fwrite("3 customer with least total sales: ~n", []),
            lists:foreach(
                fun({C, _}) ->
                    io:fwrite("~s~n", [C])
                end,
                query:customers_with_least_sales(3, Invoices)
            ),
            io:fwrite("~s~n", [Separator]),
            %%
            {Date, Total} = query:date_with_largest_total_sales(Invoices),
            io:fwrite(
                "date with largest total sales: ~s, ~.2f~n",
                [Date, Total]
            ),
            io:fwrite("~s~n", [Separator])
    end.

%%---
%%
%%---
v3(Inventories, #{invoices := InvoicesToImport, load_order := Order}) ->
    {_, _, Errors} =
        sales:save_invoices(
            #{
                to_save => InvoicesToImport,
                order => Order,
                inventories => Inventories,
                saved => dict:new()
            }
        ),
    lists:foreach(
        fun(#{invoice := Invoice, line_no := LineNo}) ->
            io:format("Line No: ~B~n", [LineNo]),
            printer:pprint(Invoice)
        end,
        Errors
    ).

%%---
%%
%%---
v4(Inventories, #{invoices := InvoicesToImport}) ->
    {_, _, Errors} =
        sales:save_invoices(
            #{
                to_save => InvoicesToImport,
                order => ascending_timestamp,
                inventories => Inventories,
                saved => dict:new()
            }
        ),
    lists:foreach(
        fun(#{invoice := Invoice, line_no := LineNo}) ->
            io:format("Line No: ~B~n", [LineNo]),
            printer:pprint(Invoice)
        end,
        Errors
    ).

%%---
%%
%%---
v5(
    Inventories,
    #{invoices := SInvoicesToImport},
    #{invoices := PInvoicesToImport}
) ->
    ok = v5_import_inventories(Inventories),
    {DocNos, Invoices} = v5_combine_and_sort_invoices(SInvoicesToImport, PInvoicesToImport),
    ImportResult = lists:map(
        fun(DocNo) ->
            case dict:fetch(DocNo, Invoices) of
                #invoice{type = sales} = Invoice ->
                    case sales:save_invoice(Invoice) of
                        {error, Errors} ->
                            {error, {DocNo, Errors}};
                        _ ->
                            ok
                    end;
                #invoice{type = purchase} = Invoice ->
                    purchase:save_invoice(Invoice)
            end
        end,
        DocNos
    ),
    Errors = lists:filter(
        fun
            ({error, _}) -> true;
            (_) -> false
        end,
        ImportResult
    ),
    lists:foreach(
        fun({error, {DocNo, DocErrors}}) ->
            lists:foreach(
                fun({LineNo, Error}) ->
                    io:format("Line No: ~B, ~p~n", [LineNo, Error])
                end,
                DocErrors
            ),
            printer:pprint(dict:fetch(DocNo, Invoices))
        end,
        Errors
    ).

%%---
%%
%%---
v5_import_inventories(Inventories) ->
    Products = dict:fetch_keys(Inventories),
    v5_import_inventories(Products, Inventories).

%%---
%%
%%---
v5_import_inventories([], _) ->
    ok;
v5_import_inventories([P | Products], Inventories) ->
    #inventory{product = P, qty = Q} = dict:fetch(P, Inventories),
    ok = inventory:move_in(P, Q),
    v5_import_inventories(Products, Inventories).

%%---
%%
%%---
v5_combine_and_sort_invoices(Invoices1, Invoices2) ->
    Invoices = dict:merge(fun(_K, V1, _V2) -> V1 end, Invoices1, Invoices2),
    UnsortedDocNos = dict:fetch_keys(Invoices),
    SortedDocNos = lists:sort(
        fun(D1, D2) ->
            #invoice{trx_ts = Ts1} = dict:fetch(D1, Invoices),
            #invoice{trx_ts = Ts2} = dict:fetch(D2, Invoices),
            Ts1 < Ts2
        end,
        UnsortedDocNos
    ),
    {SortedDocNos, Invoices}.
