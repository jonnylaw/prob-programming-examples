# Probabilistic Programming with Scala

This repository contains an implementation of the examples contained in the paper “Functional probabilistic programming for scalable Bayesian modelling” using the [Rainier](https://github.com/stripe/rainier/) modelling language.

This repository also contains implementations of forward and reverse mode automatic differentiation and a test suite. Inspired by [Noel Welsh's presentation](https://github.com/noelwelsh/fdl/blob/master/src/main/scala/fdl/Dual.scala)

To reproduce the examples from the paper, first install the [simple build tool](https://www.scala-sbt.org) for Scala then navigate to the top level directory and run `make inference`.

The plots can also be reproduced by running `make plots`.
