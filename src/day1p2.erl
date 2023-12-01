-module(day1p2).

-export([solve/1]).
-export([process_line/1]).

-spec solve(string()) -> ok.
solve(Filename) ->
  Lines = read_file(Filename),
  Result = lists:sum([process_line(L) || L <- Lines ]),
  io:format("~p~n", [Result]),
  ok.

%% two1nine         29
%% eightwothree     83
%% abcone2threexyz  13
%% xtwone3four      24
%% 4nineeightseven2 42
%% zoneight234      14
%% 7pqrstsixteen    76

%% In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281

%% one8one          11
%% oneight          18
%% oneightwo        12
process_line(Data0) ->
  Data = words_to_numbers(Data0),
  Result0 = extract_numbers(Data),
  Result =
    if
      length(Result0) > 2 -> [hd(Result0),lists:last(Result0)];
      length(Result0) =:= 1 -> [hd(Result0), hd(Result0)];
      true -> Result0
    end,
  list_to_integer(Result).

extract_numbers(Line) ->
  lists:filter(fun(C) ->
                   C >= 48 andalso C =< 57
               end, Line).

words_to_numbers(Data) ->
  WordNumber = [ {"one", "one1one"}
               , {"two", "two2two"}
               , {"three", "three3three"}
               , {"four", "four4four"}
               , {"five", "five5five"}
               , {"six", "six6six"}
               , {"seven", "seven7seven"}
               , {"eight", "eight8eight"}
               , {"nine", "nine9nine"}
               ],
  NewData = lists:foldl(fun({W,N}, Acc0) ->
                  case string:find(Acc0, W) of
                    nomatch -> Acc0;
                    _ -> lists:flatten(string:replace(Acc0, W, N, all))
                  end
              end, Data, WordNumber),
  NewData.

read_file(Filename) ->
  {ok, BinData} = file:read_file(Filename),
  Data = binary:bin_to_list(BinData),
  string:tokens(Data, "\n").

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
