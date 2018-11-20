-module(neo4j_driver).

%% API exports
-export([
  init/0,
  options/4,
  run/2
]).

-export([test/0]).

%%====================================================================
%% API functions
%%====================================================================

init() ->
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

% "parameters" : {
%       "props" : {
%         "name" : "My Node"
%       }
%     }

%%====================================================================
%% Internal functions
%%====================================================================

% make_parameter_binary(Parameter) ->
%   { Key, Value } = Parameter,
%   { list_to_binary(Key), list_to_binary(Value) }.

make_parameter_binary({ Key, Value }) ->
  { make_parameter_binary(Key), make_parameter_binary(Value) };
make_parameter_binary(List) ->
  lists:map(fun make_parameter_binary/1, List);
make_parameter_binary(String) ->
  list_to_binary(String).

% make_statement(Statement) ->
%   [
%     { <<"statement">>, list_to_binary(Statement) }
%   ].
make_statement(Statement) ->
  [
    {
      <<"statement">>,
      list_to_binary(proplists:get_value(statement, Statement))
    },
    {
      <<"parameters">>,
      lists:map(
        fun make_parameter_binary/1,
        proplists:get_value(parameters, Statement, [])
      )
    }
  ].

make_statements(Statements) ->
  A = [
    {
      <<"statements">>, lists:map(fun make_statement/1, Statements)
    }
  ],
  io:format("Sajt: ~p", A),
  A.

send_post(Url, Body) ->
  restc:request(post, json, Url, [], [], Body, []).
