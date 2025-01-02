# adpro

Repository for the course: Advanced programming (adpro)

## Commands

### Running all tests

```shell
scala-cli test .
```

### Running a single test
  
```shell
scala-cli test -- -f <exercise-name>
```

### Running REPL (Read-Eval-Print-Loop)

```shell
scala-cli repl .
```

Afterwards import necessary classes.
Often importing `adpro.*` is needed, thus write:

```scala
import adpro.<exercise-name>.*
```

## Probability

When running something in `REPL` and you want to check if the probability is correct, you can use the following:

```scala
import pigaro.*
val res = <your-expression>.sample(n)
println(res.pr(<your-condition>))
```

In `10-prob` the specific commands run were:

```scala
import pigaro.*
val res = pick(1000)
println(res.pr(Black))
println(res.pr(Red))
```
