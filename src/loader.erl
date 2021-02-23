-module(loader).
-include_lib("./stockman.hrl").
-export([load_file/1]).

load_file(FilePath) ->
    {ok, Contents} = file:read_file(FilePath),
    [_Header | Lines] = lists:filter(fun(Line) -> not string:is_empty(Line) end,
                                     string:split(Contents, "\n", all)),
    lists:foldl(fun load_file_line/2, dict:new(), Lines).

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
