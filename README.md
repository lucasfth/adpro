# adpro

Repository for the course: Advanced programming (adpro)

## Commands

### Running all tests

```shell
scala-cli test .
```

### Running a single test
  
```shell
scala-cli test . -- -f <exercise-name>
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

## Notes

### Given keyword

Something about that if it says `given` somewhere you probably need to use `with` instead of `=`.
I have no idea why.

## Quick LazyList

Often you can use below to import necessities to create a quick LazyList:

```scala
import adpro.laziness.LazyList.*
val testLazyList = from(4).take(2)
```

## Parser

To test a parser quick parser you have to import necessities and run the following:

```scala
import adpro.parsing.*
import adpro.parsing.Sliceable.*
<specific parser>.run(<some string>)
```

## Pigaro/Probability

Ensure imports and what the other thing is:

```scala
import pigaro.*
given rng: spire.random.rng.SecureJava = spire.random.rng.SecureJava.apply
```

Run following to test uniform distribution:

```scala
val samples = <func>.sample(10000)
samples.pr(<option 1>)
```

