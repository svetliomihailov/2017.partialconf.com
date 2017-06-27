---
title: Functional Technologies on the Web, pt. 1
date: 2017-05-30 16:12 UTC
tags:
---

In recent years the functional approach have been front and central with the rise of real-time UIs, cloud computing and distributed systems. Functional technologies provide a lot of solutions that can help with these challenges, but with so many choices it’s usually difficult to decide where to start first, or go next.

We [started](https://medium.com/@partialconf/announcement-2335684ba05d) Partial as a platform for exchanging knowledge and ideas. Our main goal is this new conference to become an annual gathering for the curious to learn, teach, and share.

This is Part 1 of our attempt at collecting an overview of the functional technologies used on the web. This is what **Partial** is all about. Stay tuned for more!

## [Haskell](https://www.haskell.org)

When we talk whatever functional, Haskell is the very first language we think of. From high order functions, to one of the most interesting type system out there, to the seldom understood monads, translated to every language out there without native support for them. It’s a helluva language where one can always learn something new.

The basis of what would become Haskell was outlined in 1987 at The Conference for Functional Programming Languages and Computer Architecture (Wow!). A lot of people were getting excited for the so-called “lazy functional languages”, so a committee was formed to collect the best ideas and implement them in a standard.

It wasn’t until 1990, when v1.0 of Haskell was released. In 1999, “The Haskell 98 Report“ defined the first stable and portable implementation of the language for teaching and experimenting. Improvements upon the design and the standard library brought to us Haskell 2010, which is the current implementation people around the world use.

Here’s a basic Hello World example in Haskell:

```haskell
module Main where

main :: IO ()
main = putStrLn "Hello Partial!"
```

There’s a great paper telling the history of Haskell, so if you are curious to learn more about the origins of this influential wizardry here’s [a document](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf?from=http%3A%2F%2Fresearch.microsoft.com%2F%7Esimonpj%2Fpapers%2Fhistory-of-haskell%2Fhistory.pdf) telling the story.

*Are you interested in sharing your experience with Haskell? Come to Sofia and tell us all about it! [Apply for a talk](https://goo.gl/qGfmds).*

## [Scala](https://www.scala-lang.org)

For all of you Java folks, there is a language with a fancy type system, OOP and strong functional influence.

Everything started with Funnel, a minimalistic programming language for functional nets. It was the first exploration of Martin Odersky to create a language, that combines the best practices of Object Oriented Programming and features from functional languages.

Soon after that, in 2001 Martin started working on Scala. The first version was released in 2003 and the rest, as they say, is history. Scala is part of the JVM ecosystem, so it has built-in interoperability with Java libraries and tooling.

Here’s Hello World in Scala:

```scala
object HelloWorld extends Application {
  Console.println("Hello Partial!")
}
```

Did you know there’s a [Scala implementation in JavaScript](https://www.scala-js.org), making it possible to write Scala programs that can run in your browser?

*If you are interested in presenting about Scala – [apply for a talk](https://goo.gl/qGfmds) on our Call for speakers.*

## [F# (F Sharp)](http://fsharp.org)

F# started as a port of OCaml on the .NET platform. It was designed as a strongly-typed functional language with built-in concurrency and metaprogramming features.

Its origins date back to 1998, when Don Syme joined Microsoft Research’s group in programming languages.

> “I joined the team and then other 10 of us joined the team, we were approached by a guy called James Plamondon, who started the project called Project 7, which was about getting 7 academic and 7 industrial programming languages on each side to target the .NET common language runtime and really check out if it was good enough”.

*Don Syme, [source](https://www.infoq.com/interviews/F-Sharp-Don-Syme)*

The first stable version of F# was released in May 2005, and as of 2010 it is available on Linux, macOS and Windows. It is widely used for mobile development, game development and GPU programming.

As for the web – [WebSharper](http://websharper.com) is the most popular web framework for F#. It brings reactive programming to the .NET platform and can be run in the browser thanks to [Fable](http://fable.io).

The development of the language is backed by the F# Software Foundation and Microsoft is the largest contributor.

Here’s Hello World in F#:

```fsharp
open System
Console.WriteLine("Hello Partial!")
```

*Do you run on the .NET platform? If yes, then F# is something we are really excited to talk at Partial – [send a proposal for talk](https://goo.gl/qGfmds).*

—

Thats it for part one, next week we will talk about the origins of Erlang, Elixir and Elm. [Follow us on Twitter](http://twitter.com/partialconf), or [subscribe to the newsletter](http://partialconf.com/#subscribe) for more news about the conference.

Have you considered applying for our Call for Speakers? It is **open until 16th of June**, so if you want to tell us more about Haskell, Scala, F# or anything else lambda – you can become a Partial speaker by submitting your talk.

<br />
<a href="https://goo.gl/qGfmds" class="btn-success">Become a speaker</a>
