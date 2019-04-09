-module(neo4j_driver_test).

-export([testOptions/0, test/0]).

testOptions() ->
  neo4j_driver:options("localhost", "7474", "testUser", "testPassword").

test() ->
  neo4j_driver:init(),
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
  neo4j_driver:run(testOptions(), Statements).
