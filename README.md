# Josephus
An implementation for https://en.wikipedia.org/wiki/Josephus_problem

To compile and test, use the standard `sbt compile`, `sbt test` commands.
In order to run, provide number of players `n` and skip value `k` like this: `sbt run n k`

Example for 300 players and skip 2:

```
sbt run 300 2
```


TODO/Improvements:
 * Should use data-driven tests in ScalaTest, like i would do with TestNG or JUnit
 * Should make O(k*log(n)) case work with function g() as in wikipedia