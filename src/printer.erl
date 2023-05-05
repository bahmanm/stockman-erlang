-module(printer).
-include("./stockman.hrl").
-export([pprint/1]).

pprint(#invoice{
    type = Type,
    doc_no = DocNo,
    bpartner = BPartner,
    trx_ts = Ts,
    discount = Discount,
    total = Total,
    lines = Lines
}) ->
    io:fwrite(
        "~n[~10s] [~-15s] [~-20s] [~25s]~n",
        [Type, DocNo, BPartner, Ts]
    ),
    SortedLines =
        lists:sort(
            fun(
                #invoice_line{line_no = LineNo1},
                #invoice_line{line_no = LineNo2}
            ) ->
                LineNo1 =< LineNo2
            end,
            Lines
        ),
    lists:foreach(fun pprint/1, SortedLines),
    io:fwrite("~56s~n~20B ~35.2f~n", ["------", -Discount, Total]);
pprint(#invoice_line{
    line_no = LineNo,
    product = Product,
    qty = Qty,
    price = Price,
    line_amt = LineAmt
}) ->
    io:fwrite(
        "~4B ~-20s ~6B ~10.2f ~12.2f~n",
        [LineNo, Product, Qty, Price, LineAmt]
    );
pprint(Invoices) ->
    DocNos = dict:fetch_keys(Invoices),
    lists:foreach(
        fun(DocNo) -> pprint(dict:fetch(DocNo, Invoices)) end,
        DocNos
    ).
