-module(neo4j_driver).

%% API exports
-export([
  init/0,
  options/4,
  make_statements/1,
  run/2
]).

-export([test/0, test2/0]).

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
  send_post(Url, make_statements(Statements)).

% element(2, lists:nth(2, lists:nth(1, element(2, lists:nth(2, element(4, A)))))).

test() ->
  neo4j_driver:init(),
  Options = neo4j_driver:options("localhost", "7474", "test", "valami"),
  Statements = [
    [
      { statement, "CREATE (a:Person {name: $name}) RETURN a" },
      { parameters, [ { "name", "Valaki" } ] }
    ],
    [
      { statement, "CREATE (n:Sajt)" }
    ],
    [
      { statement, "CREATE (n {props}) RETURN n" },
      { parameters, [
          { "props", [
              { "name", "My node" }
            ]
          }
        ]
      }
    ]
  ],
  neo4j_driver:run(Options, Statements).

test2() ->
  neo4j_driver:init(),
  Options = neo4j_driver:options("localhost", "7474", "neo4j", "Neo4j"),
  Statements = [
    [
      { statement, "CREATE (a:Person {name: $name}) RETURN a" },
      { parameters, [ { "name", "Valaki" } ] }
    ],
    [
      { statement, "CREATE (n:Sajt)" }
    ],
    [
      { statement, "CREATE (n {props}) RETURN n" },
      { parameters, [
          { "props", [
              { "name", "My node" }
            ]
          }
        ]
      }
    ]
  ],
  neo4j_driver:run(Options, Statements).


%%====================================================================
%% Internal functions
%%====================================================================

nested_strings_to_binary(Value) ->
  if
    is_tuple(Value) -> my_tuple_to_binary(Value);
    is_list(Value) -> my_list_to_binary(Value)
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
