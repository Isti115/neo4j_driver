-module(neo4j_driver).

%% API exports
-export([
  init/0,
  options/4,
  make_statements/1,
  run/2,
  run/3,
  get_results/1
]).

%%====================================================================
%% API functions
%%====================================================================

init() ->
  io:format("~n --- neo4j_driver:init --- ~n~n"),
  application:ensure_all_started(restc).

options(Host, Port, Username, Password) ->
  [
    { host, Host },
    { port, Port },
    { username, Username },
    { password, Password }
  ].

run(Options, Statements) ->
  run(Options, Statements, false).

run(Options, Statements, Debug) ->
  Url = (
    "http://" ++
    proplists:get_value(username, Options) ++
    ":" ++
    proplists:get_value(password, Options) ++
    "@" ++
    proplists:get_value(host, Options) ++
    ":" ++
    proplists:get_value(port, Options) ++
    "/db/data/transaction/commit"
  ),
  MadeStatements = make_statements(Statements),

  if
    Debug -> own_debug("Sending statements", [MadeStatements]);
    true -> ok
  end,

  Response = send_post(Url, MadeStatements),
  nested_binary_to_string(Response).

get_results(Response) ->
  {Status, StatusCode, Headers, [
    {"results", Results},
    {"errors", Errors}
  ]} = Response,

  case Errors of
    [] -> Results;
    ErrorList -> throw({ query_error, ErrorList })
  end.

% element(2, lists:nth(2, lists:nth(1, element(2, lists:nth(2, element(4, A)))))).

%%====================================================================
%% Internal functions
%%====================================================================

own_debug(Text, Data) ->
    io:format("~n\e[32m\e[41m##### ~p: ~p #####\e[0m~n", [Text, Data]).

%

nested_strings_to_binary(Value) ->
  if
    is_tuple(Value) -> my_tuple_to_binary(Value);
    is_list(Value) -> my_list_to_binary(Value);
    true -> Value
  end.

my_tuple_to_binary(Tuple) ->
  { Key, Value } = Tuple,
  { list_to_binary(Key), nested_strings_to_binary(Value) }.

my_list_to_binary(List) ->
  case lists:any(fun is_tuple/1, List) of
    true -> lists:map(fun my_tuple_to_binary/1, List);
    false ->
      case lists:any(fun is_list/1, List) of
        true -> lists:map(fun my_list_to_binary/1, List);
        false -> list_to_binary(List)
      end
  end.

%

nested_binary_to_string(Value) ->
  if
    is_tuple(Value) -> my_tuple_to_string(Value);
    is_list(Value) -> my_list_to_string(Value);
    is_binary(Value) -> binary_to_list(Value);
    true -> Value
  end.

my_tuple_to_string(Tuple) ->
  list_to_tuple(my_list_to_string(tuple_to_list(Tuple))).

my_list_to_string(List) ->
  lists:map(fun nested_binary_to_string/1, List).

%

make_statement(Statement) ->
  [
    {
      <<"statement">>,
      list_to_binary(proplists:get_value(statement, Statement))
    }
  ]
  ++
  case proplists:is_defined(parameters, Statement) of
    true ->
      [
        {
          <<"parameters">>,
          nested_strings_to_binary(proplists:get_value(parameters, Statement, []))
        }
      ];

    false -> []
  end.

make_statements(Statements) ->
  [
    {
      <<"statements">>, lists:map(fun make_statement/1, Statements)
    }
  ].

send_post(Url, Body) ->
  restc:request(post, json, Url, [], [], Body, []).
