-module(inventory).

-behaviour(gen_server).

-include("./stockman.hrl").

%% api
-export([start/0]).
-export([move_in/2, move_out/2, available_qty/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2]).

%% macros
-define(SERVER, ?MODULE).

%% types
-record(state, {items :: inventories()}).
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
-spec move_in(Product :: product(), Qty :: integer()) -> ok.

move_in(Product, Qty) ->
    gen_server:call(?SERVER, {move_in, {Product, Qty}}).

%%---
-spec move_out(Product :: product(), Qty :: integer()) ->
    ok | {error, Reason :: term()}.

move_out(Product, Qty) ->
    gen_server:call(?SERVER, {move_out, {Product, Qty}}).

%%---
-spec available_qty(Product :: product()) ->
    {ok, Qty :: integer()} | {error, no_such_product}.

available_qty(Product) ->
    gen_server:call(?SERVER, {available_qty, {Product}}).

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
-spec handle_call
    (
        {move_in, {Product :: product(), Qty :: integer()}},
        From :: any(),
        State :: state()
    ) ->
        {reply, ok, NewState :: state()};
    (
        {move_out, {Product :: product(), Qty :: integer()}},
        From :: any(),
        State :: state()
    ) ->
        {reply, ok | {error, insufficient_inventory | unknown_product}, NewState :: state()};
    (
        {available_qty, Product :: product()},
        From :: any(),
        State :: state()
    ) ->
        {reply, {ok, Qty :: integer()} | {error, unknown_product}, NewState :: state()}.

handle_call({move_in, {P, Q}}, _, #state{items = Items} = S) ->
    Items1 =
        case dict:is_key(P, Items) of
            true ->
                Items;
            _ ->
                dict:store(
                    P,
                    #inventory{product = P, qty = 0},
                    Items
                )
        end,
    #inventory{qty = Current} = I = dict:fetch(P, Items1),
    NewI = I#inventory{qty = (Current + Q)},
    NewItems = dict:store(P, NewI, Items1),
    NewState = S#state{items = NewItems},
    {reply, ok, NewState};
handle_call({move_out, {P, Q}}, _, #state{items = Items} = S) ->
    case dict:is_key(P, Items) of
        true ->
            #inventory{qty = Current} = I = dict:fetch(P, Items),
            if
                (Current < Q) ->
                    {reply, {error, insufficient_qty}, S};
                true ->
                    NewI = I#inventory{qty = (Current - Q)},
                    NewItems = dict:store(P, NewI, Items),
                    NewState = S#state{items = NewItems},
                    {ok, NewState}
            end;
        _ ->
            {reply, {error, unknown_product}, S}
    end;
handle_call({available_qty, P}, _, #state{items = Items} = S) ->
    case dict:is_key(P, Items) of
        true ->
            #inventory{qty = Q} = dict:fetch(P, Items),
            {reply, {ok, Q}, S};
        _ ->
            {reply, {error, unknown_product}, S}
    end.

%%---
%% @private
%%---
handle_cast(_Request, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% tests
%%------------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sales_shipment_test() ->
    Is0 = dict:new(),
    Is1 = dict:store("p1", #inventory{product = "p1", qty = 10}, Is0),
    State1 = #state{items = Is1},
    {ok, #state{items = Is2} = _State2} =
        impl_purchase_receipt({"p1", 10}, State1),
    #inventory{product = "p1", qty = 20} = dict:fetch("p1", Is2).

-endif.
