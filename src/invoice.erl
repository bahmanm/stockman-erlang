-module(invoice).

-include("./stockman.hrl").

-behaviour(gen_server).

%% api
-export([start/0, save/2]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2]).

%% callbacks
-callback before_validate_invoice(Invoice :: invoice()) -> ok | {error, _}.
-callback before_validate_invoice_line(Line :: invoice_line()) -> ok | {error, _}.
-callback before_store_invoice(Invoice :: invoice()) -> ok | {error, _}.
-callback before_store_invoice_line(Line :: invoice_line()) -> ok | {error, _}.

%% types
-record(state, {items :: dict:dict(doc_no(), invoice())}).
-type state() :: #state{}.
-type before_validate_invoice_function() :: fun((Invoice :: invoice()) -> ok | {error, _}).
-type before_validate_invoice_line_function() :: fun(
    (InvoiceLine :: invoice_line()) -> ok | {error, _}
).
-type invoice_error() :: {error, [{LineNo :: line_no(), Error :: any()}]} | {error, Error :: any()}.

%% macros
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------------------------------------
%% api
%%--------------------------------------------------------------------------------------------------

%%---
%%
%%---
-spec start() ->
    {ok, Pid :: pid()}
    | {error, Error :: {already_started, pid()}}
    | {error, Error :: term()}
    | ignore.

start() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

%%---
%%
%%---
-spec save(Invoice :: invoice(), Subtype :: module()) ->
    ok | invoice_error().

save(Invoice, Subtype) ->
    gen_server:call(?SERVER, {save, Invoice, Subtype}).

%%--------------------------------------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------------------------------------

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
-spec handle_call(
    {save, Invoice :: invoice(), Subtype :: module()}, _From :: any(), State :: state()
) ->
    {reply, ok | invoice_error(), NewState :: state()}.

handle_call({save, Invoice, Subtype}, _, State) ->
    {Status, NewState} = do_save(Invoice, Subtype, State),
    {reply, Status, NewState};
handle_call(_, _, State) ->
    {reply, ok, State}.

%%---
%% @private
%%---
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------------------------------------
%% private functions
%%--------------------------------------------------------------------------------------------------

%%---
%%
%%---
-spec do_save(Invoice :: invoice(), Subtype :: module(), State :: state()) ->
    {ok | invoice_error(), NewState :: state()}.

do_save(Invoice, Subtype, State) ->
    case
        validate_invoice(
            Invoice,
            fun Subtype:before_validate_invoice/1,
            fun Subtype:before_validate_invoice_line/1
        )
    of
        ok -> store(Invoice, Subtype, State);
        Error -> {Error, State}
    end.

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
    #invoice{total = Total, discount = Discount, lines = Lines} = Invoice,
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

%%---
%%
%%---
-spec store(Invoice :: invoice(), Subtype :: module(), State :: state()) ->
    {ok | invoice_error(), NewState :: state()}.

store(#invoice{doc_no = DocNo, lines = Lines} = Invoice, Subtype, State) ->
    case process_invoice_lines(Lines, Subtype, State) of
        {ok, #state{items = Items}} ->
            {ok, State#state{items = dict:store(DocNo, Invoice, Items)}};
        Error ->
            Error
    end.

%%---
%%
%%---
process_invoice_lines([], _, State) ->
    {ok, State};
process_invoice_lines([Line | Lines], Subtype, State) ->
    case Subtype:before_store_invoice_line(Line) of
        ok ->
            process_invoice_lines(Lines, Subtype, State);
        Error ->
            {Error, state}
    end.
