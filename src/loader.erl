-module(loader).
-include_lib("./stockman.hrl").
-export([load_file/2]).

-spec load_file(invoice | inventory, Filepath :: string()) ->
          dict:dict(string(), #inventory{})
              | #{load_order => [string()],
                  invoices => dict:dict(string(), #invoice{})}.
load_file(invoice, Filepath) ->
    Lines = load_lines(Filepath),
    #{load_order := Order} = Result =
        lists:foldl(fun load_invoice_file_line/2,
                    #{invoices => dict:new(), load_order => []},
                    Lines),
    Result#{load_order := lists:reverse(Order)};

load_file(inventory, Filepath) ->
    Lines = load_lines(Filepath),
    lists:foldl(fun load_inventory_file_line/2, dict:new(), Lines).

load_lines(Filepath) ->
    {ok, Contents} = file:read_file(Filepath),
    [_Header | Lines] = lists:filter(fun(Line) -> not string:is_empty(Line) end,
                                     string:split(Contents, "\n", all)),
    Lines.

load_invoice_file_line(Line, #{invoices := Invoices, load_order := Order}=Result) ->
    [Type, DocNo, BPartner, Date, Total, Discount,
     LineNo, Product, Qty, Price, LineAmt] = string:split(Line, ",", all),
    InvoiceLine = new_invoice_line(LineNo, Product, Qty, Price, LineAmt),
    case dict:find(DocNo, Invoices) of
        {ok, #invoice{lines=Lines}=Invoice} ->
            Result#{invoices := dict:store(DocNo,
                                           Invoice#invoice{lines=[InvoiceLine|Lines]},
                                           Invoices)};
        _ ->
            TypeAtom = case Type of
                           "sales" -> sales;
                           "purchase" -> purchase
                       end,
            Invoice = new_invoice(TypeAtom, DocNo, BPartner, Date, Discount, Total),
            Result#{invoices := dict:store(DocNo,
                                           Invoice#invoice{lines=[InvoiceLine]},
                                           Invoices),
                    load_order := [DocNo|Order]}
    end.

load_inventory_file_line(Line, Inventories) ->
    [Product, Qty] = string:split(Line, ",", all),
    I = new_inventory(Product, Qty),
    dict:store(Product, I, Inventories).

new_invoice(Type, DocNo, BPartner, TrxTs, Discount, Total) ->
    try
        #invoice{type=Type,
                 doc_no=DocNo,
                 bpartner=BPartner,
                 trx_ts=TrxTs,
                 discount=erlang:binary_to_integer(Discount),
                 total=erlang:binary_to_float(Total),
                 lines=[]}
    catch
        error:badarg -> throw({invalid_invoice,
                               {Type, DocNo, BPartner, TrxTs, Discount, Total}})
    end.

new_invoice_line(LineNo, Product, Qty, Price, LineAmt) ->
    try
        #invoice_line{line_no=erlang:binary_to_integer(LineNo),
                      product=Product,
                      qty=erlang:binary_to_integer(Qty),
                      price=erlang:binary_to_float(Price),
                      line_amt=erlang:binary_to_float(LineAmt)}
    catch
        error:badarg -> throw({invalid_invoice_line,
                               {LineNo, Product, Qty, Price, LineAmt}})
    end.

new_inventory(Product, Qty) ->
    try
        #inventory{product=Product,
                   qty=erlang:binary_to_integer(Qty)}
    catch
        error:badarg ->
            throw({invalid_inventory, {Product, Qty}})
    end.
