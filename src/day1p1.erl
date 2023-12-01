-module(day1p1).

-export([solve/1]).
-export([process_line/1]).

%% @doc Input is a filepath
-spec solve(string()) -> ok.
solve(Filename) ->
  Lines = read_file(Filename),
  Result = lists:sum([process_line(L) || L <- Lines ]),
  io:format("~p~n", [Result]),
  ok.

%% 1abc2
%% pqr3stu8vwx
%% a1b2c3d4e5f
%% treb7uchet
%% In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142
process_line(Data) ->
  Result0 = lists:filter(fun(C) ->
                            C >= 48 andalso C =< 57
                        end, Data),
  Result =
    if
      length(Result0) > 2 -> [hd(Result0),lists:last(Result0)];
      length(Result0) =:= 1 -> [hd(Result0), hd(Result0)];
      true -> Result0
    end,
  list_to_integer(Result).

read_file(Filename) ->
  {ok, BinData} = file:read_file(Filename),
  Data = binary:bin_to_list(BinData),
  string:tokens(Data, "\n").

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
