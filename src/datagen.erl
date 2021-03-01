-module(datagen).
-include("./stockman.hrl").
-export([main/1]).

main(Args) ->
    OptSpec =
        [{outdir, $o, "output-directory", {string, "."},
          "directory to store the generated csv files"},
         {n_inventories, undefined, "inventories", integer,
          "how many inventory records to generate"},
         {n_sinvoices, undefined, "sales-invoices", integer,
          "how many sales invoices to generate"},
         {n_customers, undefined, "customers", integer,
          "how many customers to generate"},
         {n_invoice_lines, undefined, "invoice-lines", {integer, 10},
          "limit the number of lines per invoice"}],
    case getopt:parse(OptSpec, Args) of
        {ok, {OptionsTList, _}} ->
            case maps:from_list(OptionsTList) of
                #{outdir := _, n_inventories := _,
                  n_sinvoices := _, n_customers := _}=Options ->
                    generate(Options);
                _ ->
                    io:format("~s", [getopt:usage(OptSpec, "datagen")])
            end;
        {error, Details} ->
            io:format("~s~n~p~n", [getopt:usage(OptSpec, "datagen"),
                                   Details])
    end.

generate(#{outdir := Outdir, n_inventories := NInventories,
           n_customers := NCustomers, n_sinvoices := NSInvoices}=Options) ->
    Inventories = gen_inventory(NInventories),
    InventoriesText = inventories_to_csv(Inventories),
    file:write_file(filename:join(Outdir, "inventory.csv"), InventoriesText),
    %
    Customers = gen_customers(NCustomers),
    Invoices = gen_invoice(NSInvoices, {Customers, Inventories, Options}),
    InvoicesText = invoices_to_csv(Invoices),
    file:write_file(filename:join(Outdir, "invoices.csv"), InvoicesText).

%%%%%%%%%%%%%

gen_timestamp() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    {MegaDelta, Delta, MicroDelta} =
        {gen_integer(0, 120), gen_integer(0, Secs), gen_integer(0, MicroSecs)},
    Datetime = {MegaSecs - MegaDelta, Secs - Delta, MicroSecs - MicroDelta},
    {{Y, Mo, D}, {H, Mi, S}} = calendar:now_to_datetime(Datetime),
    io_lib:format("~4B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B",
                 [Y, Mo, D, H, Mi, S]).

gen_float() ->
    gen_float(0.0, 1000.0).

gen_float(Min, Max) when Min =< Max ->
    F = round(rand:uniform() * rand:uniform(gen_integer(10, 100)) * 100) / 100,
    if
        F =< Max andalso F >= Min ->
            F;
        true ->
            gen_float(Min, Max)
    end.

gen_integer() ->
    gen_integer(1, 9999).

gen_integer(Min, Max) when Min =< Max ->
    N = floor((rand:uniform(120) * rand:uniform())
              + (rand:uniform(40) / rand:uniform())),
    if
        N =< Max andalso N >= Min ->
            N;
        true ->
            gen_integer(Min, Max)
    end.

%%%%%%%%%%%%%

dict_rand_iterator(Dict) ->
    fun() ->
            Keys = dict:fetch_keys(Dict),
            Key = lists:nth(gen_integer(1, length(Keys)), Keys),
            {Value, NextDict} = dict:take(Key, Dict),
            {Value, dict_rand_iterator(NextDict)}
    end.

%%%%%%%%%%%%%

gen_inventory(NRecords) ->
    gen_inventory(NRecords, dict:new()).

gen_inventory(0, Inventories) ->
    Inventories;

gen_inventory(NRemaining, Inventories) ->
    Product = io_lib:format("P-~4..0B", [gen_integer()]),
    case dict:is_key(Product, Inventories) of
        true ->
            gen_inventory(NRemaining, Inventories);
        _ ->
            Qty = gen_integer(1, 1000) + gen_integer(1, 1000),
            NewInventories = dict:store(Product,
                                      #inventory{product=Product, qty=Qty},
                                      Inventories),
            gen_inventory(NRemaining-1, NewInventories)
    end.

inventories_to_csv(Inventories) ->
    lists:reverse(
      inventories_to_csv(
        dict:to_list(Inventories), [<<"product,qty\n">>])).

inventories_to_csv([],Result) ->
    Result;

inventories_to_csv([{_, #inventory{product=Product,qty=Qty}}|T], Result) ->
    S = list_to_binary(io_lib:format("~s,~B~n", [Product, Qty])),
    inventories_to_csv(T, [S|Result]).

%%%%%%%%%%%%%

gen_customers(N) ->
    lists:reverse(gen_customers(N, [])).

gen_customers(0, Result) ->
    Result;

gen_customers(NRemaining, Result) ->
    C = io_lib:format("C-~4..0B", [gen_integer()]),
    case lists:any(fun(C1) -> C =:= C1 end, Result) of
        true ->
            gen_customers(NRemaining, Result);
        _ ->
            gen_customers(NRemaining-1, [C|Result])
    end.

%%%%%%%%%%%%%

gen_invoice_line(N, Inventories) ->
    gen_invoice_line(N, dict_rand_iterator(Inventories), []).

gen_invoice_line(0, _, Result) ->
    Result;

gen_invoice_line(NRemaining, NextInventoryF, Result) ->
    {#inventory{product=Product}, F} = NextInventoryF(),
    Qty = gen_integer(1, 50),
    Price = gen_float(),
    Amt = Qty * Price,
    gen_invoice_line(NRemaining-1, F,
                     [#invoice_line{line_no=NRemaining, product=Product, qty=Qty,
                                    price=Price, line_amt=Amt}|Result]).

gen_invoice(N, {Customers, Inventories, Options}) ->
    gen_invoice(N, {Customers, Inventories, Options}, []).

gen_invoice(0, _, Result) ->
    lists:reverse(Result);

gen_invoice(NRemaining, {Customers, Inventories, Options}, Result) ->
    DocNo = io_lib:format("SI-~4..0B", [gen_integer()]),
    case lists:any(fun(#invoice{doc_no=IDocNo}) ->
                           IDocNo =:= DocNo
                   end,
                   Result) of
        true ->
            gen_invoice(NRemaining, {Customers, Inventories, Options}, Result);
        _ ->
            Customer = lists:nth(gen_integer(1,length(Customers)), Customers),
            Timestamp = gen_timestamp(),
            NLines = gen_integer(1, maps:get(n_invoice_lines, Options)),
            Lines = gen_invoice_line(NLines, Inventories),
            SumLineAmt = lists:foldl(fun(#invoice_line{line_amt=Amt}, Acc) ->
                                             Amt + Acc
                                     end,
                                     0.0,
                                     Lines),
            Discount = gen_integer(1, 50),
            Total = (SumLineAmt * Discount) / 100,
            Result1 = [#invoice{doc_no=DocNo, trx_ts=Timestamp, customer=Customer,
                                discount=Discount,total=Total,lines=Lines}|Result],
            gen_invoice(NRemaining-1, {Customers, Inventories, Options}, Result1)
    end.

invoices_to_csv(Invoices) ->
    list_to_binary(invoices_to_csv(Invoices, [])).

invoices_to_csv([], Result) ->
    [<<"docNo,customer,timestamp,total,discount,lineNo,product,qty,price,lineAmt\n">> |
     lists:reverse(Result)];

invoices_to_csv([#invoice{doc_no=DocNo, trx_ts=TrxTs, customer=Customer,
                         discount=Discount,total=Total,lines=Lines}|T], Result) ->
    Prefix = io_lib:format("~s,~s,~s,~.2f,~B",
                           [DocNo, TrxTs, Customer, Total, Discount]),
    S = lists:map(fun(#invoice_line{line_no=LineNo, product=Product, qty=Qty,
                                    price=Price, line_amt=Amt}) ->
                          io_lib:format("~s,~B,~s,~B,~.2f,~.2f\n",
                                        [Prefix, LineNo, Product, Qty, Price, Amt])
                  end,
                  Lines),
    invoices_to_csv(T, [S|Result]).
