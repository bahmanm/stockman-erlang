-module(invoice).

-include("./stockman.hrl").

-export([validate_invoice/1, validate_invoice/2]).

-type additional_validation_function() :: fun((InvoiceLine :: #invoice_line{}) -> ok | {error, _}).

%%------------------------------------------------------------------------------
%% api
%%------------------------------------------------------------------------------

%%---
%%
%%---

-spec validate_invoice(Invoice :: #invoice{}) ->
    ok | {error, [{LineNo :: integer(), Error :: any()}] | Error :: any()}.

validate_invoice(Invoice) ->
    validate_invoice(Invoice, fun(_) -> ok end).

%%---
%%
%%---

-spec validate_invoice(
    Invoice :: #invoice{}, AdditionalValidationFunction :: additional_validation_function()
) ->
    ok | {error, [{LineNo :: integer(), Error :: any()}] | Error :: any()}.

validate_invoice(
    #invoice{type = sales, total = Total, discount = Discount, lines = Lines},
    AdditionalValidationFunction
) ->
    InvalidLines = lists:filtermap(
        fun(#invoice_line{line_no = LineNo} = Line) ->
            case validate_invoice_line(Line, AdditionalValidationFunction) of
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
            api
        %%--------            {error, InvalidLines}
    end.

%%------------------------------------------------------------------------------
%% private
%%------------------------------------------------------------------------------

%%---
%%
%%---
-spec validate_invoice_line(
    InvoiceLine :: #invoice_line{}, AdditionalValidationFunction :: additional_validation_function()
) ->
    ok | {error, Error :: any()}.

validate_invoice_line(
    #invoice_line{qty = Qty, price = Price, line_amt = LineAmt} = Line,
    AdditionalValidationFunction
) ->
    if
        Qty =< 0 ->
            {error, invalid_qty};
        Price =< 0 ->
            {error, invalid_price};
        (LineAmt =< 0) or (LineAmt /= (Qty * Price)) ->
            {error, invalid_line_amt};
        true ->
            AdditionalValidationFunction(Line)
    end.
