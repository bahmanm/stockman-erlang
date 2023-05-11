-module(sales).

-behaviour(gen_server).

-include("./stockman.hrl").

-export([save_invoices/1]).

%% api
-export([start/0]).
-export([save_invoice/1]).

%% gen_server
-export([init/1]).

%% macros
-define(SERVER, ?MODULE).

%% types
-record(state, {items :: invoices()}).
-type state() :: #state{}.

%%------------------------------------------------------------------------------
%% api
%%------------------------------------------------------------------------------
-spec start() ->
    {ok, Pid :: pid()}
    | {error, Error :: {already_started, pid()}}
    | {error, Error :: term()}
    | ignore.

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%---
-spec save_invoice(Invoice :: #invoice{}) ->
    ok | {error, list({LineNo :: int, Error :: any()})}.

save_invoice(_Invoice) ->
    ok.

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

%%---
%% @private
%%---
-spec init(Args :: term()) -> {ok, State :: term()}.

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{items = dict:new()}}.

%%---
%% @private
%%---
-spec handle_call({save_invoice, Invoice :: #invoice{}}) ->
    {reply, ok} | {reply, error, list({LineNo :: int, Error :: any()})}.

handle_call({save_invoice, Invoice}) ->
    ok.

%%------------------------------------------------------------------------------
%% private
%%------------------------------------------------------------------------------

%%---
%% @private
%%---
-spec validate_invoice_line(InvoiceLine :: #invoice_line{}) ->
    ok | {error, Error :: any()}.

validate_invoice_line(#invoice_line{product = P, qty = Qty, price = Price, line_amt = LineAmt}) ->
    if
        Qty =< 0 ->
            {error, invalid_qty};
        Price =< 0 ->
            {error, invalid_price};
        (LineAmt =< 0) or (LineAmt /= (Qty * Price)) ->
            {error, invalid_line_amt};
        true ->
            case inventory:available_qty(P) of
                {ok, QAvailable} when Qty =< QAvailable ->
                    ok;
                {ok, _} ->
                    {error, insufficient_inventory};
                {error, _} = Error ->
                    Error
            end
    end.

-spec validate_invoice(Invoice :: #invoice{}) ->
    ok | {error, [{LineNo :: integer(), Error :: any()}] | Error :: any()}.

validate_invoice(#invoice{type = sales, total = Total, discount = Discount, lines = Lines}) ->
    InvalidLines = lists:filtermap(
        fun(#invoice_line{line_no = LineNo} = Line) ->
            case validate_invoice_line(Line) of
                {error, Error} -> {true, {LineNo, Error}};
                _ -> false
            end
        end,
        Lines
    ),
    case InvalidLines of
        [] ->
            if
                (Discount < 0) or (Discount > 100) ->
                    {error, invalid_discount};
                true ->
                    CalculatedTotal =
                        (lists:foldl(
                            fun(#invoice_line{line_amt = LineAmt}, Sum) ->
                                Sum + LineAmt
                            end,
                            0,
                            Lines
                        ) *
                            (100 - Discount)) / 100,
                    if
                        Total /= CalculatedTotal ->
                            {error, invalid_total};
                        true ->
                            ok
                    end
            end;
        _ ->
            {error, InvalidLines}
    end.

-spec save_invoices(#{
    to_save => invoices(),
    inventories => inventories(),
    saved => invoices(),
    order => ascending_timestamp | [doc_no()]
}) ->
    {inventories(), invoices(), [ImportError]}
when
    ImportError :: #{invoice => #invoice{}, line_no => string()}.

save_invoices(#{
    to_save := ToSave,
    order := ascending_timestamp,
    inventories := Inventories,
    saved := Saved
}) ->
    Order = sort_invoices_by_trx_ts(ToSave),
    save_invoices(ToSave, Order, Inventories, Saved);
save_invoices(#{
    to_save := ToSave,
    order := Order,
    inventories := Inventories,
    saved := Saved
}) ->
    save_invoices(ToSave, Order, Inventories, Saved).

%%------------------------------------------------------------------------------
%% private
%%------------------------------------------------------------------------------

update_inventory(Product, Qty, Inventories) ->
    case dict:find(Product, Inventories) of
        {ok, #inventory{qty = Q} = I} when (Q + Qty) >= 0 ->
            {ok, dict:store(Product, I#inventory{qty = Q + Qty}, Inventories)};
        _ ->
            error
    end.

save_invoices(ToSave, Order, Inventories, Saved) ->
    lists:foldl(
        fun(DocNo, {Ies, Ios, Errs}) ->
            Io = dict:fetch(DocNo, ToSave),
            case save_invoice(Io, Ies, Ios) of
                {ok, Ies1, Ios1} ->
                    {Ies1, Ios1, Errs};
                {error, {line_no, LineNo}} ->
                    {Ies, Ios, [#{invoice => Io, line_no => LineNo} | Errs]}
            end
        end,
        {Inventories, Saved, []},
        Order
    ).

save_invoice(
    #invoice{type = Type, doc_no = DocNo, lines = Lines} = Invoice,
    Inventories,
    Invoices
) ->
    case save_invoice_line(Type, Lines, Inventories) of
        {ok, Inventories1} ->
            Invoices1 = dict:store(DocNo, Invoice, Invoices),
            {ok, Inventories1, Invoices1};
        {error, _} = E ->
            E
    end.

save_invoice_line(_, [], Inventories) ->
    {ok, Inventories};
save_invoice_line(
    InvoiceType,
    [#invoice_line{product = P, qty = Q} = Line | Lines],
    Inventories
) ->
    Q1 =
        case InvoiceType of
            sales ->
                inventory:move_out(P, Q),
                -Q;
            purchase ->
                inventory:move_in(P, Q),
                Q
        end,
    case update_inventory(P, Q1, Inventories) of
        {ok, Inventories1} ->
            save_invoice_line(InvoiceType, Lines, Inventories1);
        _ ->
            {error, {line_no, Line#invoice_line.line_no}}
    end.

sort_invoices_by_trx_ts(Invoices) ->
    Unsorted =
        dict:fold(
            fun(DocNo, #invoice{trx_ts = Ts}, Acc) ->
                [{DocNo, Ts} | Acc]
            end,
            [],
            Invoices
        ),
    Sorted =
        lists:sort(
            fun({_, Ts1}, {_, Ts2}) ->
                Ts1 =< Ts2
            end,
            Unsorted
        ),
    lists:map(
        fun({DocNo, _}) -> DocNo end,
        Sorted
    ).

%%------------------------------------------------------------------------------
%% tests
%%------------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

update_inventory_test() ->
    I0 = dict:from_list(
        [{"p1", #inventory{product = "p1", qty = 0}}]
    ),

    {ok, I1} = update_inventory("p1", 10, I0),
    #inventory{qty = 10} = dict:fetch("p1", I1),

    {ok, I2} = update_inventory("p1", 5, I1),
    #inventory{qty = 15} = dict:fetch("p1", I2),

    {ok, I3} = update_inventory("p1", -14, I2),
    #inventory{qty = 1} = dict:fetch("p1", I3),

    error = update_inventory("p1", -2, I3).

save_invoice_line_test() ->
    I0 = dict:from_list(
        [{"p1", #inventory{product = "p1", qty = 10}}]
    ),
    Lines0 = [#invoice_line{line_no = 10, product = "p1", qty = 5}],

    {ok, I1} = save_invoice_line(sales, Lines0, I0),
    #inventory{qty = 5} = dict:fetch("p1", I1),

    Lines1 = [#invoice_line{line_no = 1, product = "p1", qty = 8}],
    {error, {line_no, 1}} = save_invoice_line(sales, Lines1, I1).

save_invoice_test() ->
    I0 = dict:from_list(
        [
            {"p1", #inventory{product = "p1", qty = 10}},
            {"p2", #inventory{product = "p2", qty = 20}}
        ]
    ),
    Invoices0 = dict:new(),
    Invoice0 = #invoice{
        doc_no = "i1",
        type = sales,
        lines = [#invoice_line{line_no = 1, product = "p1", qty = 8}]
    },

    {ok, I1, Invoices1} = save_invoice(Invoice0, I0, Invoices0),
    #inventory{qty = 2} = dict:fetch("p1", I1),
    #invoice{doc_no = "i1"} = dict:fetch("i1", Invoices1),

    Invoice1 = #invoice{
        doc_no = "i2",
        type = sales,
        lines = [
            #invoice_line{line_no = 1, product = "p1", qty = 2}#invoice_line{
                line_no = 2, product = "p2", qty = 23
            }
        ]
    },
    {error, {line_no, 2}} = save_invoice(Invoice1, I1, Invoices1).

save_invoices_test() ->
    I0 = dict:from_list(
        [
            {"p1", #inventory{product = "p1", qty = 10}},
            {"p2", #inventory{product = "p2", qty = 20}}
        ]
    ),
    ToSave = dict:from_list([
        {"i1", #invoice{
            doc_no = "i1",
            type = sales,
            lines = [
                #invoice_line{
                    line_no = 1, product = "p1", qty = 8
                },
                #invoice_line{
                    line_no = 2, product = "p2", qty = 2
                }
            ]
        }},
        {"i2", #invoice{
            doc_no = "i2",
            type = sales,
            lines = [
                #invoice_line{
                    line_no = 1, product = "p2", qty = 20
                }
            ]
        }}
    ]),

    {I1, Saved, Errors} = save_invoices(#{
        to_save => ToSave,
        order => ["i1", "i2"],
        inventories => I0,
        saved => dict:new()
    }),
    #inventory{qty = 2} = dict:fetch("p1", I1),
    #inventory{qty = 18} = dict:fetch("p2", I1),
    true = dict:is_key("i1", Saved),
    false = dict:is_key("i2", Saved),
    [#{invoice := #invoice{doc_no = "i2"}, line_no := 1}] = Errors.

sort_invoices_by_trx_ts_test() ->
    Invoices =
        dict:from_list(
            [
                {"i1", #invoice{doc_no = "i1", trx_ts = "10"}},
                {"i2", #invoice{doc_no = "i2", trx_ts = "14"}},
                {"i3", #invoice{doc_no = "i2", trx_ts = "15"}},
                {"i4", #invoice{doc_no = "i2", trx_ts = "11"}}
            ]
        ),
    ["i1", "i4", "i2", "i3"] = sort_invoices_by_trx_ts(Invoices).

validate_invoice_line__test() ->
    {error, invalid_qty} = validate_invoice_line(#invoice_line{
        product = "p1", qty = 0, price = 10.0, line_amt = 0
    }),
    {error, invalid_price} = validate_invoice_line(#invoice_line{
        product = "p1", qty = 10, price = 0.0, line_amt = 0
    }),
    {error, invalid_line_amt} = validate_invoice_line(#invoice_line{
        product = "p1", qty = 10, price = 10.0, line_amt = 0
    }),
    {error, invalid_line_amt} = validate_invoice_line(#invoice_line{
        product = "p1", qty = 10, price = 10.0, line_amt = 90.0
    }),
    meck:new(inventory),
    meck:expect(inventory, available_qty, fun(_) -> {ok, 9} end),
    {error, insufficient_inventory} = validate_invoice_line(#invoice_line{
        product = "p1", qty = 10, price = 10.0, line_amt = 100.0
    }).

validate_invoice__test() ->
    InvalidLine1 = #invoice_line{line_no = 10, product = "p1", qty = 0, price = 10, line_amt = 0},
    InvalidLine2 = #invoice_line{line_no = 20, product = "p1", qty = 10, price = 0, line_amt = 0},
    InvalidInvoice1 = #invoice{
        type = sales,
        bpartner = "c1",
        discount = 0,
        total = 100,
        lines = [InvalidLine1, InvalidLine2]
    },
    {error, [{10, invalid_qty}, {20, invalid_price}]} = validate_invoice(InvalidInvoice1),
    %
    ValidLine1 = #invoice_line{line_no = 10, product = "p1", qty = 2, price = 5, line_amt = 10},
    ValidLine2 = #invoice_line{line_no = 20, product = "p2", qty = 4, price = 2, line_amt = 8},
    InvalidInvoice2 = InvalidInvoice1#invoice{
        lines = [ValidLine1, ValidLine2], discount = 0, total = 5
    },
    {error, invalid_total} = validate_invoice(InvalidInvoice2),
    %
    InvalidInvoice3 = InvalidInvoice2#invoice{
        lines = [ValidLine1, ValidLine2], discount = 110, total = 18
    },
    {error, invalid_discount} = validate_invoice(InvalidInvoice3).

-endif.
