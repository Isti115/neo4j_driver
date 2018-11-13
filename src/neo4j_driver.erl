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
    {host, Host},
    {port, Port},
    {username, Username},
    {password, Password}
  ].

run(Options, Statement) ->
  St = make_statement(Statement),
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
  send_post(Url, St).

test() ->
  neo4j_driver:init(),
  Options = neo4j_driver:options("localhost", "7474", "test", "valami"),
  neo4j_driver:run(Options,"CREATE (n)").

%%====================================================================
%% Internal functions
%%====================================================================

make_statement(S) ->
  [
    {
      <<"statements">>, [
        [
          {<<"statement">>, list_to_binary(S)}
        ]
      ]
    }
  ].

send_post(Url, Body) ->
  restc:request(post, json, Url, [], [], Body, []).
