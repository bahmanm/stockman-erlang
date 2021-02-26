-module(sales).
-include("./stockman.hrl").

update_inventory(Product, Qty, Inventories) ->
    case dict:find(Product, Inventories) of
        {ok, #inventory{qty=Q}=I} when (Q + Qty) >= 0 ->
            {ok, dict:store(Product, I#inventory{qty=Q+Qty}, Inventories)};
        _ ->
            error
    end.

save_invoice(#invoice{doc_no=DocNo, lines=Lines}=Invoice, Inventories, Invoices) ->
    case save_invoice_line(Lines, Inventories) of
        {ok, Inventories1} ->
            Invoices1 = dict:store(DocNo, Invoice, Invoices),
            {ok, Inventories1, Invoices1};
        {error, _}=E ->
            E
    end.

save_invoice_line([], Inventories) ->
    {ok, Inventories};

save_invoice_line([#invoice_line{product=P, qty=Q}=Line|Lines], Inventories) ->
    case update_inventory(P, -Q, Inventories) of
        {ok, Inventories1} ->
            save_invoice_line(Lines, Inventories1);
        _ ->
            {error, {line_no, Line#invoice_line.line_no}}
    end.


%%% %%%%%%%%
%%% tests
%%% %%%%%%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

update_inventory_test() ->
    I0 = dict:from_list(
           [{"p1", #inventory{product="p1", qty=0}}]
          ),

    {ok, I1} = update_inventory("p1", 10, I0),
    #inventory{qty=10} = dict:fetch("p1", I1),

    {ok, I2} = update_inventory("p1", 5, I1),
    #inventory{qty=15} = dict:fetch("p1", I2),

    {ok, I3} = update_inventory("p1", -14, I2),
    #inventory{qty=1} = dict:fetch("p1", I3),

    error = update_inventory("p1", -2, I3).

save_invoice_line_test() ->
    I0 = dict:from_list(
           [{"p1", #inventory{product="p1", qty=10}}]
          ),
    Lines0 = [#invoice_line{line_no=10, product="p1", qty=5}],

    {ok, I1} = save_invoice_line(Lines0, I0),
    #inventory{qty=5} = dict:fetch("p1", I1),

    Lines1 = [#invoice_line{line_no=1, product="p1", qty=8}],
    {error, {line_no, 1}} = save_invoice_line(Lines1, I1).

save_invoice_test() ->
    I0 = dict:from_list(
           [{"p1", #inventory{product="p1", qty=10}},
            {"p2", #inventory{product="p2", qty=20}}]
          ),
    Invoices0 = dict:new(),
    Invoice0 = #invoice{doc_no="i1",
                        lines=[#invoice_line{line_no=1, product="p1", qty=8}]},

    {ok, I1, Invoices1} = save_invoice(Invoice0, I0, Invoices0),
    #inventory{qty=2} = dict:fetch("p1", I1),
    #invoice{doc_no="i1"} = dict:fetch("i1", Invoices1),

    Invoice1 = #invoice{doc_no="i2",
                        lines=[#invoice_line{line_no=1, product="p1", qty=2}
                               #invoice_line{line_no=2, product="p2", qty=23}]},
    {error, {line_no, 2}} = save_invoice(Invoice1, I1, Invoices1).

-endif.
