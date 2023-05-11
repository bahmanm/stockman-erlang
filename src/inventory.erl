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
    gen_server:call(?SERVER, {available_qty, Product}).

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
    ItemsWithP =
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
    #inventory{qty = CurrentQty} = CurrentInventory = dict:fetch(P, ItemsWithP),
    NewInventory = CurrentInventory#inventory{qty = (CurrentQty + Q)},
    NewItems = dict:store(P, NewInventory, ItemsWithP),
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
                    {reply, ok, NewState}
            end;
        _ ->
            {reply, {error, unknown_product}, S}
    end;
handle_call({available_qty, P}, _, #state{items = Items} = S) ->
    io:format("~p ~p ~p", [P, dict:is_key(P, Items), Items]),
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

move_out__test() ->
    Is0 = dict:new(),
    Is1 = dict:store("p1", #inventory{product = "p1", qty = 10}, Is0),
    State1 = #state{items = Is1},
    {reply, ok, State2} = handle_call({move_out, {"p1", 2}}, from, State1),
    {reply, {ok, 8}, State2} = handle_call({available_qty, "p1"}, from, State2).

move_out__insufficient_inventory_test() ->
    Is0 = dict:new(),
    Is1 = dict:store("p1", #inventory{product = "p1", qty = 1}, Is0),
    State1 = #state{items = Is1},
    {reply, {error, insufficient_qty}, State1} = handle_call({move_out, {"p1", 2}}, from, State1),
    {reply, {ok, 1}, State1} = handle_call({available_qty, "p1"}, from, State1).

move_out__unknown_product_test() ->
    Is0 = dict:new(),
    Is1 = dict:store("p1", #inventory{product = "p1", qty = 1}, Is0),
    State1 = #state{items = Is1},
    {reply, {error, unknown_product}, State1} = handle_call(
        {move_out, {"DOES NOT EXIST", 2}}, from, State1
    ),
    {reply, {ok, 1}, State1} = handle_call({available_qty, "p1"}, from, State1).

move_in__test() ->
    Is0 = dict:new(),
    Is1 = dict:store("p1", #inventory{product = "p1", qty = 1}, Is0),
    State1 = #state{items = Is1},
    {reply, ok, State2} = handle_call({move_in, {"p1", 5}}, from, State1),
    {reply, {ok, 6}, State2} = handle_call({available_qty, "p1"}, from, State2).

move_in__new_product_test() ->
    Is0 = dict:new(),
    State1 = #state{items = Is0},
    {reply, ok, State2} = handle_call({move_in, {"p1", 5}}, from, State1),
    {reply, {ok, 5}, State2} = handle_call({available_qty, "p1"}, from, State2).

available_qty__test() ->
    Is0 = dict:new(),
    Is1 = dict:store("p1", #inventory{product = "p1", qty = 5}, Is0),
    State1 = #state{items = Is1},
    {reply, {ok, 5}, State1} = handle_call({available_qty, "p1"}, from, State1).

available_qty__unknown_product_test() ->
    Is0 = dict:new(),
    State1 = #state{items = Is0},
    {reply, {error, unknown_product}, State1} = handle_call({available_qty, "p1"}, from, State1).

-endif.
