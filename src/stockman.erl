-module(stockman).

-export([main/1]).

-record(invoice_line, { doc_no, customer, date, total, discount,
                        line_no, product, qty, price, line_amt }).
-record(invoice, {doc_no, customer, date, discount, total, lines}).


main([FilePath|[]]) ->
    process(FilePath),
    erlang:halt(0).

process(FilePath) ->
    {ok, Contents} = file:read_file(FilePath),
    [_Header | Lines] = string:split(Contents, "\n", all),
    InvoiceLines = lists:map(fun line_to_invoiceline/1, Lines),
    Invoices = lists:foldl(fun group_invoicelines/2, dict:new(), InvoiceLines),
    lists:foreach(fun(DocNo) ->
                         pprint(dict:fetch(DocNo, Invoices))
                 end, dict:fetch_keys(Invoices)).

line_to_invoiceline(Line) ->
    [DocNo, Customer, Date, Total, Discount, LineNo, Product, Qty, Price, LineAmt] = string:split(Line, ",", all),
    #invoice_line{ doc_no = DocNo, customer = Customer, date = Date, total = Total,
                   discount = Discount, line_no = LineNo, product = Product, qty = Qty,
                   price = Price, line_amt = LineAmt }.

group_invoicelines(#invoice_line{doc_no=DocNo}=InvoiceLine, Invoices) ->
    case dict:find(DocNo, Invoices) of
        {ok, #invoice{lines = Lines}=Invoice} ->
            dict:store(DocNo, Invoice#invoice{ lines = [InvoiceLine|Lines] }, Invoices);
        _ ->
            #invoice_line{ customer = Customer, date = Date, discount = Discount, total = Total } = InvoiceLine,
            Invoice = #invoice { doc_no = DocNo, customer = Customer, date = Date, discount = Discount, total = Total, lines = [InvoiceLine] },
            dict:store(DocNo, Invoice, Invoices)
    end.

pprint(#invoice{ doc_no = DocNo, customer = Customer, date = Date, discount = Discount, total = Total, lines = Lines }) ->
    io:fwrite("~-15s~-20s~-15s~n", [DocNo, Customer, Date]),
    lists:foreach(fun pprint/1, Lines),
    io:fwrite("~20s~20s~n", [Discount, Total]);

pprint(#invoice_line{ line_no = LineNo, product = Product, qty = Qty, price = Price, line_amt = LineAmt }) ->
    io:fwrite("~4s~-20s~5s~8s~8s~n", [LineNo, Product, Qty, Price, LineAmt]).
