# Numbers

[![Build Status](https://travis-ci.org/Sciss/Numbers.svg?branch=master)](https://travis-ci.org/Sciss/Numbers)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/de.sciss/numbers_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/de.sciss/numbers_2.11)

## statement

Numbers is a library for the Scala programming language which provides various useful numeric functions and type 
enrichment operators. Many of the unary and binary operators are adapted from the SuperCollider language, and 
Numbers is included in the [ScalaCollider](http://github.com/Sciss/ScalaCollider/) project. This library is 
(C)opyright 2013&ndash;2018 by Hanns Holger Rutz. All rights reserved. It is released under the
[GNU Lesser General Public License](http://github.com/Sciss/Numbers/blob/master/LICENSE) and comes with 
absolutely no warranties. To contact the author, send an email to `contact at sciss.de`

## linking

    "de.sciss" %% "numbers"  % v

The current version `v` is `"0.2.0"`.

## building

The project builds with [sbt](http://www.scala-sbt.org/) against Scala 2.12, 2.11.

To compile `sbt compile`. To test, `sbt test`. For the API docs, `sbt doc`.

## example

```scala
import de.sciss.numbers.Implicits._

2.0.sqrt.ampDb  // 3.01
0.1.linExp(0, 1, 100, 10000)  // 158.5
```
    
## changes

As of version 0.2.0, we use more idiomatic Scala names with camel-case for composite operators, e.g.
`ampDb` instead of `ampdb`, `linLin` instead of `linlin`, `absDif` instead of `absdif`.