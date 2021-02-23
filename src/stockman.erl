-module(stockman).
-include("./stockman.hrl").
-export([main/1]).

main([FilePath|[]]) ->
    Invoices = loader:load_file(FilePath),
    printer:pprint(Invoices),
    erlang:halt(0).
