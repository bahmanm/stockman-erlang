-module(purchase).

-behaviour(invoice).

-include("./stockman.hrl").

%% api
-export([save_invoice/1]).

%% invoice callbacks
-export([
    before_validate_invoice/1,
    before_validate_invoice_line/1,
    before_store_invoice/1,
    before_store_invoice_line/1
]).

%%--------------------------------------------------------------------------------------------------
%% api
%%--------------------------------------------------------------------------------------------------

%%---
%%
%%---
-spec save_invoice(Invoice :: invoice()) -> ok | {error, _}.

save_invoice(#invoice{type = purchase} = Invoice) ->
    invoice:save(Invoice, ?MODULE).

%%--------------------------------------------------------------------------------------------------
%% invoice callbacks
%%--------------------------------------------------------------------------------------------------

%%---
%%
%%---
-spec before_validate_invoice(Invoice :: invoice()) -> ok | {error, _}.
before_validate_invoice(_Invoice) ->
    ok.

%%---
%%
%%---
-spec before_validate_invoice_line(Line :: invoice_line()) -> ok | {error, _}.
before_validate_invoice_line(_Line) ->
    ok.

%%---
%%
%%---
-spec before_store_invoice(Invoice :: invoice()) -> ok | {error, _}.
before_store_invoice(_Invoice) ->
    ok.

%%---
%%
%%---
-spec before_store_invoice_line(Line :: invoice_line()) -> ok | {error, _}.
before_store_invoice_line(#invoice_line{product = P, qty = Q}) ->
    inventory:move_in(P, Q).
