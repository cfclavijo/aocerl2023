-module(day2p2).

-export([solve/1]).
-export([process_line/1]).

-include_lib("eunit/include/eunit.hrl").

-spec solve(string()) -> ok.
solve(Filename) ->
  Lines = read_file(Filename),
  Result = lists:sum([process_line(L) || L <- Lines ]),
  io:format("~p~n", [Result]),
  Result.

%% Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
%% Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
%% Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
%% Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
%% Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

%% Sum Game 1,2 and 5
%% Result 8
%% only 12 red cubes, 13 green cubes, and 14 blue cubes
process_line(Data) ->
  {_GameId, GameSets} = parse_line(Data),
  %% io:format("~p~n ~p ~p~n", [Data, GameId, GameSets]),
  calculate_gamesets(GameSets).

calculate_gamesets(GameSets) ->
  {MinR, MinG, MinB}
    = lists:foldl(fun(GS, {Rmax, Gmax, Bmax}) ->
                      R = maps:get("red", GS, 0),
                      G = maps:get("green", GS, 0),
                      B = maps:get("blue", GS, 0),
                      RN = max(R, Rmax),
                      GN = max(G, Gmax),
                      BN = max(B, Bmax),
                      {RN, GN, BN}
                  end, {0, 0, 0}, GameSets),
  MinR * MinG * MinB.

parse_line(Data) ->
  [GameInfo, GameSets0] = string:tokens(Data, ":"),
  GameId = list_to_integer(extract_numbers(GameInfo)), % to add
  GameSets = parse_gamesets(GameSets0),
  {GameId, GameSets}.

parse_gamesets(Data) ->
  GameSets = string:tokens(Data, ";"),
  [parse_gameset(GS) || GS <- GameSets].

parse_gameset(Data0) ->
  Data = re:split(Data0, "[ ,]", [{return,list}]),
  game_to_map(Data, #{}).

game_to_map([], Result) -> Result;
game_to_map(["" | T], Result) -> game_to_map(T, Result);
game_to_map([Val0, Color | T], Result0) ->
  Val = list_to_integer(Val0),
  OldVal = maps:get(Color, Result0, 0),
  Result = maps:put(Color, OldVal + Val, Result0),
  game_to_map(T, Result).

extract_numbers(Line) ->
  lists:filter(fun(C) ->
                   C >= 48 andalso C =< 57
               end, Line).

read_file(Filename) ->
  {ok, BinData} = file:read_file(Filename),
  Data = binary:bin_to_list(BinData),
  string:tokens(Data, "\n").

-ifdef(TEST).

solve_test() ->
  R = solve("data/day2p1_dummy.txt"),
  ?assertEqual(8, R).

-endif.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
