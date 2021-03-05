-module(datagen).
-include("./stockman.hrl").
-export([main/1]).

-type inventories() :: dict:dict(string(), #inventory{}).
-type invoices() :: dict:dict(string(), #invoice{}) | undefined.

-record(data,
        {customers :: [string()],
         vendors :: [string()],
         inventories :: inventories(),
         sinvoices :: invoices(),
         pinvoices :: invoices()}).

main(Args) ->
    OptSpec =
        [{outdir, $o, "output-directory", {string, "."},
          "directory to store the generated csv files"},
         {n_inventories, undefined, "inventories", integer,
          "how many inventory records to generate"},
         {n_customers, undefined, "customers", {integer, 0},
          "limit the number of unique customers"},
         {n_vendors, undefined, "vendors", {integer, 0},
          "limit the number of unique vendors"},
         {n_sinvoices, undefined, "sales-invoices", {integer, 0},
          "how many sales invoices to generate"},
         {n_invoice_lines, undefined, "invoice-lines", {integer, 10},
          "limit the number of lines per invoice"},
         {n_pinvoices, undefined, "purchase-invoices", {integer, 0},
          "how many purchase invoices to generate"}],
    case getopt:parse(OptSpec, Args) of
        {ok, {OptionsTList, _}} ->
            case maps:from_list(OptionsTList) of
                #{n_inventories := _}=Options ->
                    generate(Options);
                _ ->
                    io:format("~s", [getopt:usage(OptSpec, "datagen")])
            end;
        {error, Details} ->
            io:format("~s~n~p~n", [getopt:usage(OptSpec, "datagen"),
                                   Details])
    end.

generate(#{outdir := Outdir,
           n_customers := NCustomers,
           n_vendors := NVendors,
           n_inventories := NInventories,
           n_sinvoices := NSInvoices,
           n_pinvoices := NPInvoices,
           n_invoice_lines := NLines}) ->
    Data0 = #data{
               customers = gen_customers(NCustomers),
               vendors = gen_vendors(NVendors),
               inventories = gen_inventories(NInventories)},
    Data1 = Data0#data{
              sinvoices = gen_sinvoices(NSInvoices, NLines, Data0)},
    Data2 = Data1#data{
              pinvoices = gen_pinvoices(NPInvoices, NLines, Data1)},
    dump(Data2, Outdir).

%%%%%%%%%%%%%

dump(#data{inventories=Inventories,
           sinvoices=SInvoices,
           pinvoices=PInvoices},
     Outdir) ->
    file:write_file(filename:join(Outdir, "inventories.csv"),
                    inventories_to_csv(Inventories)),
    case SInvoices of
        undefined -> ok;
        _ -> file:write_file(filename:join(Outdir, "sinvoices.csv"),
                             invoices_to_csv(SInvoices))
    end,
    case PInvoices of
        undefined -> ok;
        _ -> file:write_file(filename:join(Outdir, "pinvoices.csv"),
                             invoices_to_csv(PInvoices))
    end.

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

randpick(L) ->
    lists:nth(gen_integer(1, length(L)),
              L).

%%%%%%%%%%%%%

-spec dict_rand_iterator(Dict :: dict:dict(any(), Value)) -> Fun when
      Fun :: fun(() -> {Value, Fun}).

dict_rand_iterator(Dict) ->
    fun() ->
            Keys = dict:fetch_keys(Dict),
            Key = lists:nth(gen_integer(1, length(Keys)), Keys),
            {Value, NextDict} = dict:take(Key, Dict),
            {Value, dict_rand_iterator(NextDict)}
    end.

%%%%%%%%%%%%%

gen_inventories(N) ->
    GenerateInventories =
        fun GenInventories(0, Acc) ->
                Acc;
            GenInventories(M, Acc) ->
                P = io_lib:format("P-~4..0B", [gen_integer()]),
                case dict:is_key(P, Acc) of
                    true ->
                        GenInventories(M, Acc);
                    _ ->
                        Qty = gen_integer(1, 1000) + gen_integer(1, 1000),
                        Inventory = #inventory{product=P, qty=Qty},
                        GenInventories(M-1, dict:store(P, Inventory, Acc))
                end
        end,
    GenerateInventories(N, dict:new()).

inventories_to_csv(Inventories) ->
    lists:reverse(dict:fold(fun(P, #inventory{qty=Q}, Acc) ->
                                    S = io_lib:format("~s,~B~n", [P, Q]),
                                    [list_to_binary(S)|Acc]
                            end,
                            [<<"product,qty\n">>],
                           Inventories)).

%%%%%%%%%%%%%

gen_customers(N) ->
    gen_bpartner(N, $C).

gen_vendors(N) ->
    gen_bpartner(N, $V).

-spec gen_bpartner(N :: integer(), Prefix :: char()) -> [string()] | [].

gen_bpartner(N, Prefix) ->
    GenBPartners =
        fun F(0, Acc) ->
                Acc;
            F(M, Acc) ->
                P = io_lib:format("~c-~4..0B", [Prefix, gen_integer()]),
                case lists:any(fun(Q) -> Q =:= P end, Acc) of
                    true ->
                        F(M, Acc);
                    _ ->
                        F(M-1, [P|Acc])
                end
        end,
    GenBPartners(N, []).

%%%%%%%%%%%%%

gen_sinvoice_docno(Invoices) ->
    gen_docno("SI", Invoices).

gen_pinvoice_docno(Invoices) ->
    gen_docno("PI", Invoices).

-spec gen_docno(Prefix :: string(), Invoices :: invoices()) -> string().
gen_docno(Prefix, Invoices) ->
    DocNo = io_lib:format("~s-~4..0B", [Prefix, gen_integer()]),
    case dict:is_key(DocNo, Invoices) of
        true -> gen_docno(Prefix, Invoices);
        _ -> DocNo
    end.

%%%%%%%%%%%%%

gen_invoices(_, 0, _, _) ->
    undefined;

gen_invoices(Type, N, MaxNLines, Data) ->
    {DocNoGenerator, BPartners} =
        case Type of
            sales -> {fun gen_sinvoice_docno/1, Data#data.customers};
            purchase -> {fun gen_pinvoice_docno/1, Data#data.vendors}
        end,
    ComputeTotal =
        fun(Lines, Discount) ->
                SumLineAmt =
                    lists:foldl(fun(#invoice_line{line_amt=Amt}, Acc) ->
                                        Amt + Acc
                                end,
                                0.0,
                                Lines),
                (SumLineAmt * Discount) / 100
        end,
    GenerateInvoices =
        fun GenInvoices(0, Invoices) ->
                Invoices;
           GenInvoices(M, Invoices) ->
                DocNo = DocNoGenerator(Invoices),
                BPartner = randpick(BPartners),
                Timestamp = gen_timestamp(),
                NLines = gen_integer(1, MaxNLines),
                Lines = gen_invoice_lines(NLines, Data#data.inventories),
                Discount = gen_integer(1, 50),
                Total = ComputeTotal(Lines, Discount),
                Invoice = #invoice{
                             type=Type, doc_no=DocNo, trx_ts=Timestamp,
                             bpartner=BPartner, discount=Discount,
                             total=Total, lines=Lines},
                GenInvoices(M-1, dict:store(DocNo, Invoice, Invoices))
        end,
    GenerateInvoices(N, dict:new()).

gen_sinvoices(N, NLines, Data) ->
    gen_invoices(sales, N, NLines, Data).

gen_pinvoices(N, NLines, Data) ->
    gen_invoices(purchase, N, NLines, Data).

%%%%%%%%%%%%%

gen_invoice_lines(N, Inventories) ->
    NextInventory = dict_rand_iterator(Inventories),
    GenerateInvoiceLines =
        fun GenInvoiceLines(0, Lines, _) ->
                Lines;
            GenInvoiceLines(M, Lines, F) ->
                {#inventory{product=Product}, NextF} = F(),
                Qty = gen_integer(1, 50),
                Price = gen_float(),
                Amt = Qty * Price,
                Line = #invoice_line{line_no=M, product=Product, qty=Qty,
                                     price=Price, line_amt=Amt},
                GenInvoiceLines(M-1, [Line|Lines], NextF)
        end,
    GenerateInvoiceLines(N, [], NextInventory).

%%%%%%%%%%%%%

invoices_to_csv(Invoices) ->
    HeaderLine =
        list_to_binary(
          lists:join(",",
                     ["docType", "docNo", "bPartner", "timestamp",
                      "total", "discount",
                      "lineNo", "product", "qty", "price", "lineAmt"
                      "\n"])),
    LinesToCsv =
        fun F([], _, Acc) ->
                Acc;
            F([#invoice_line{line_no=LineNo, product=Product,
                             qty=Qty, price=Price, line_amt=Amt}|Lines],
              Prefix, Acc) ->
                S = io_lib:format("~s,~B,~s,~B,~.2f,~.2f~n",
                                  [Prefix, LineNo, Product, Qty, Price, Amt]),
                F(Lines, Prefix, [S|Acc])
        end,
    InvoiceToCsv =
        fun(_, #invoice{type=Type, doc_no=DocNo, trx_ts=TrxTs,
                        bpartner=BPartner, discount=Discount, total=Total,
                        lines=Lines}, Acc) ->
                Prefix =
                    io_lib:format("~s,~s,~s,~s,~.2f,~B",
                                  [Type, DocNo, BPartner, TrxTs, Total, Discount]),
                LinesToCsv(Lines, Prefix, Acc)
        end,
    [HeaderLine|
     lists:reverse(dict:fold(InvoiceToCsv, [], Invoices))].
