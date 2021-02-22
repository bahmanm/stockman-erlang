-module(stockman).

-export([main/1]).

-record(invoice_line, { line_no, product, qty, price, line_amt }).
-record(invoice, { doc_no, customer, date, discount, total, lines }).

main([FilePath|[]]) ->
    load_file(FilePath),
    erlang:halt(0).

load_file(FilePath) ->
    {ok, Contents} = file:read_file(FilePath),
    [_Header | Lines] = lists:filter(fun(Line) -> not string:is_empty(Line) end,
                                     string:split(Contents, "\n", all)),
    Invoices = lists:foldl(fun load_file_line/2, dict:new(), Lines),
    lists:foreach(fun(DocNo) -> pprint(dict:fetch(DocNo, Invoices)) end,
                  dict:fetch_keys(Invoices)).

load_file_line(Line, Invoices) ->
    [DocNo, Customer, Date, Total, Discount,
     LineNo, Product, Qty, Price, LineAmt] = string:split(Line, ",", all),
    InvoiceLine = new_invoice_line(LineNo, Product, Qty, Price, LineAmt),
    case dict:find(DocNo, Invoices) of
        {ok, #invoice{ lines = Lines } = Invoice} ->
            dict:store(DocNo,
                       Invoice#invoice{ lines = [InvoiceLine|Lines] },
                       Invoices);
        _ ->
            Invoice = new_invoice(DocNo, Customer, Date, Discount, Total),
            dict:store(DocNo,
                       Invoice#invoice{ lines = [InvoiceLine] },
                       Invoices)
    end.

new_invoice(DocNo, Customer, Date, Discount, Total) ->
    try
        #invoice{ doc_no = DocNo,
                  customer = Customer,
                  date = Date,
                  discount = erlang:binary_to_integer(Discount),
                  total = erlang:binary_to_float(Total),
                  lines = [] }
    catch
        error:badarg -> throw({ invalid_invoice,
                                { DocNo, Customer, Date, Discount, Total } })
    end.

new_invoice_line(LineNo, Product, Qty, Price, LineAmt) ->
    try
        #invoice_line{ line_no = erlang:binary_to_integer(LineNo),
                       product = Product,
                       qty = erlang:binary_to_integer(Qty),
                       price = erlang:binary_to_float(Price),
                       line_amt = erlang:binary_to_float(LineAmt) }
    catch
        error:badarg -> throw({ invalid_invoice_line,
                                { LineNo, Product, Qty, Price, LineAmt } })
    end.

pprint(#invoice{ doc_no = DocNo, customer = Customer, date = Date,
                 discount = Discount, total = Total, lines = Lines }
      ) ->
    io:fwrite("~n[~-15s] [~-20s] [~15s]~n", [DocNo, Customer, Date]),
    SortedLines = lists:sort(
                    fun(#invoice_line{ line_no = LineNo1 },
                        #invoice_line{ line_no = LineNo2 }
                       ) ->
                            LineNo1 =< LineNo2
                    end,
                    Lines),
    lists:foreach(fun pprint/1, SortedLines),
    io:fwrite("~56s~n~20B ~35.2f~n", ["------", -Discount, Total]);

pprint(#invoice_line{ line_no = LineNo, product = Product, qty = Qty,
                      price = Price, line_amt = LineAmt }
      ) ->
    io:fwrite("~4B ~-20s ~6B ~10.2f ~12.2f~n", [LineNo, Product, Qty, Price, LineAmt]).
