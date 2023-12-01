-module(day1p1).

-export([solve/1]).

%% @doc Input is a filepath
-spec solve(string()) -> ok.
solve(Filename) ->
  io:format("~s~n", [Filename]),
  Values = read_file(Filename),
  ct:pal("~p~nValues=~p", [{?MODULE, ?FUNCTION_NAME, ?LINE}, Values]),
  ok.

read_file(Filename) ->
  {ok, BinData} = file:read_file(Filename),
  Data = binary:bin_to_list(BinData),
  string:tokens(Data, "\n").

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
