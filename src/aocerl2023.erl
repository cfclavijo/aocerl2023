-module(aocerl2023).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
  io:format("Args: ~p~n", [Args]),
  Cmd = args_to_cmd(Args),
  execute_cmd(Cmd),
  erlang:halt(0).

run_puzzle(PuzzleName, InputParams) ->
  erlang:apply(PuzzleName, solve, InputParams).

%%====================================================================
%% Internal functions
%%====================================================================

execute_cmd(cmd_help) -> print_help();
execute_cmd(cmd_list_puzzles) -> print_available_puzzles();
execute_cmd({cmd_puzzle, {PuzzleName, InputParams}}) ->
  run_puzzle(PuzzleName, InputParams).

args_to_cmd(Args) ->
  {CmdHelp, CmdList, CmdPuzz} =
    lists:foldl(fun(Arg, {CmdH, CmdL, CmdP}) ->
                    Acc = case Arg of
                            "-h" -> {true, CmdL, CmdP};
                            "-p" -> {CmdH, true, CmdP};
                            _ ->
                              case check_cmd_puzzle_args(Args) of
                                false -> {CmdH, CmdL, false};
                                ParsedPzzlCmd -> {CmdH, CmdL, ParsedPzzlCmd}
                              end
                          end,
                    Acc
                end, {false, false, false}, Args),
  if
    CmdHelp -> cmd_help;
    CmdList -> cmd_list_puzzles;
    CmdPuzz =/= false ->
      {ok, PuzzleName, InputParams} = CmdPuzz,
      {cmd_puzzle, {PuzzleName, InputParams}};
    true -> cmd_help
  end.

check_cmd_puzzle_args([]) -> false;
check_cmd_puzzle_args([_]) -> false;
check_cmd_puzzle_args([H | T]) ->
  case lists:keyfind(H, 1, available_puzzles()) of
    false -> false;
    {_, PuzzleAtom} -> {ok, PuzzleAtom, T}
  end.

available_puzzles() ->
  [ {"day1p1", day1p1}
  , {"day1p2", day1p2}
  , {"day2p1", day2p1}
  , {"day2p2", day2p2}
  ].

print_available_puzzles() ->
  Puzzles = [P || {P, _} <- available_puzzles()],
  io:format("~s~n", [lists:join("\n", Puzzles)]).

print_help() ->
  HelpMsg =
    "aocerl2023 is a base of code with solutions written in Erlang for Advent of Code 2023."
    "\n\nUsage: aocerl2023 [-h] [-p] <puzzle> <input file>"
    "\n\n\t-h  Print this help."
    "\n\t-p  Print a list of available puzzles.",
  io:format("~s~n", [HelpMsg]).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
