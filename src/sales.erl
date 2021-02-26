-module(sales).
-include("./stockman.hrl").

update_inventory(Product, Qty, Inventories) ->
    case dict:find(Product, Inventories) of
        {ok, #inventory{qty=Q}=I} when (Q + Qty) >= 0 ->
            {ok, dict:store(Product, I#inventory{qty=Q+Qty}, Inventories)};
        _ ->
            error
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

-endif.
