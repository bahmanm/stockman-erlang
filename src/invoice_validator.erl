-module(invoice_validator).

-include("./stockman.hrl").

-export([validate/2]).

-callback before_validate_invoice(Invoice :: #invoice{}) -> ok | {error, any()}.
-callback before_validate_invoice_line(Line :: #invoice_line{}) -> ok | {error, any()}.

-type before_validate_invoice_function() :: fun((Invoice :: #invoice{}) -> ok | {error, _}).
-type before_validate_invoice_line_function() :: fun(
    (InvoiceLine :: #invoice_line{}) -> ok | {error, _}
).

%%--------------------------------------------------------------------------------------------------
%% api
%%--------------------------------------------------------------------------------------------------

%%---
%%
%%---

-spec validate(Invoice :: #invoice{}, SubValidator :: module()) ->
    ok | {error, [{LineNo :: integer(), Error :: any()}] | Error :: any()}.

validate(Invoice, SubValidator) ->
    validate_invoice(
        Invoice,
        fun SubValidator:before_validate_invoice/1,
        fun SubValidator:before_validate_invoice_line/1
    ).

%%---
%%
%%---

-spec validate_invoice(
    Invoice :: #invoice{},
    BeforeValidateInvoice :: before_validate_invoice_function(),
    BeforeValidateInvoiceLine :: before_validate_invoice_line_function()
) ->
    ok | {error, [{LineNo :: integer(), Error :: any()}] | Error :: any()}.

validate_invoice(
    #invoice{type = sales, total = Total, discount = Discount, lines = Lines} = Invoice,
    BeforeValidateInvoice,
    BeforeValidateInvoiceLine
) ->
    InvalidLines = lists:filtermap(
        fun(#invoice_line{line_no = LineNo} = Line) ->
            case validate_invoice_line(Line, BeforeValidateInvoiceLine) of
                {error, Error} -> {true, {LineNo, Error}};
                _ -> false
            end
        end,
        Lines
    ),
    case InvalidLines of
        [] ->
            case BeforeValidateInvoice(Invoice) of
                ok ->
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
                Error ->
                    Error
            end;
        _ ->
            {error, InvalidLines}
    end.

%%------------------------------------------------------------------------------
%% private
%%------------------------------------------------------------------------------

%%---
%%
%%---
-spec validate_invoice_line(
    InvoiceLine :: #invoice_line{},
    BeforeValidateInvoiceLine :: before_validate_invoice_line_function()
) ->
    ok | {error, Error :: any()}.

validate_invoice_line(
    #invoice_line{qty = Qty, price = Price, line_amt = LineAmt} = Line,
    BeforeValidateInvoiceLine
) ->
    case BeforeValidateInvoiceLine(Line) of
        ok ->
            if
                Qty =< 0 ->
                    {error, invalid_qty};
                Price =< 0 ->
                    {error, invalid_price};
                (LineAmt =< 0) or (LineAmt /= (Qty * Price)) ->
                    {error, invalid_line_amt};
                true ->
                    ok
            end;
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% tests
%%------------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%---

validate_invoice_line__test() ->
    {error, invalid_qty} = validate_invoice_line(
        #invoice_line{
            product = "p1", qty = 0, price = 10.0, line_amt = 0
        },
        fun(_) -> ok end
    ),
    {error, invalid_price} = validate_invoice_line(
        #invoice_line{
            product = "p1", qty = 10, price = 0.0, line_amt = 0
        },
        fun(_) -> ok end
    ),
    {error, invalid_line_amt} = validate_invoice_line(
        #invoice_line{
            product = "p1", qty = 10, price = 10.0, line_amt = 0
        },
        fun(_) -> ok end
    ),
    {error, invalid_line_amt} = validate_invoice_line(
        #invoice_line{
            product = "p1", qty = 10, price = 10.0, line_amt = 90.0
        },
        fun(_) -> ok end
    ).

%%---

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
    {error, [{10, invalid_qty}, {20, invalid_price}]} = validate_invoice(InvalidInvoice1, fun(_) -> ok end, fun(_) -> ok end),
    %
    ValidLine1 = #invoice_line{line_no = 10, product = "p1", qty = 2, price = 5, line_amt = 10},
    ValidLine2 = #invoice_line{line_no = 20, product = "p2", qty = 4, price = 2, line_amt = 8},
    InvalidInvoice2 = InvalidInvoice1#invoice{
        lines = [ValidLine1, ValidLine2], discount = 0, total = 5
    },
    {error, invalid_total} = validate_invoice(InvalidInvoice2, fun(_) -> ok end, fun(_) -> ok end),
    %
    InvalidInvoice3 = InvalidInvoice2#invoice{
        lines = [ValidLine1, ValidLine2], discount = 110, total = 18
    },
    {error, invalid_discount} = validate_invoice(InvalidInvoice3, fun(_) -> ok end, fun(_) -> ok end).

-endif.
