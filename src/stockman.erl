-module(stockman).
-include("./stockman.hrl").
-export([main/1]).

main([FilePath|[]]) ->
    Invoices = loader:load_file(FilePath),
    printer:pprint(Invoices),
    erlang:halt(0).

total_sales(Invoices) ->
    dict:fold(fun(_, #invoice{ total=Total }, TotalSales) ->
                      TotalSales + Total
              end,
              0.0,
              Invoices).



%%%
%%% tests
%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

total_sales_test() ->
    Invoices = load_file("./test/resources/sales-invoices-tiny.csv"),
    148271.56 = total_sales(Invoices).

-endif.
