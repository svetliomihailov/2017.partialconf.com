---
title: Functional Technologies on the Web, pt. 2
date: 2017-06-06 11:29 UTC
tags:
---

Last week we [covered](http://partialconf.com/blog/functional-technologies-on-the-web-pt1) the origins of Haskell, Scala and F#.

Today we continue with part 2 of our overview of functional technologies.

## [Erlang](https://www.erlang.org/)

Let's start this one with a short, but inspirational video:

<div class="video-container">
    <iframe width="980" height="551" src="https://www.youtube.com/embed/uKfKtXYLG78" frameborder="0" allowfullscreen></iframe>
</div>

Erlang was devised to improve the development of telephony related software in the labs of Ericsson in 1982. The very first versions were implemented in Prolog, but the need for better performance pushed engineers from Ericsson to start a complete rewrite by 1992. This brought the BEAM virtual machine, the heart of modern Erlang and, of course, the main driving force behind Elixir.

Up until the late 90’s Erlang was with a closed source, but Ericsson recongised that in order to grow its popularity they need to share it with the community.

Fun fact: Erlang is named after danish matematician and engineer [Agner Krarup Erlang](https://en.wikipedia.org/wiki/Agner_Krarup_Erlang)

Here’s the vision for language, as outlined by Joe Armstrong, Erlang’s co-inventor:

- Everything is a process.
- Processes are strongly isolated.
- Process creation and destruction is a lightweight operation.
- Message passing is the only way for processes to interact.
- Processes have unique names.
- If you know the name of a process you can send it a message.
- Processes share no resources.
- Error handling is non-local.
- Processes do what they are supposed to do or fail.

Or simply put: [“If Java is 'write once, run anywhere', then Erlang is 'write once, run forever'.”](https://www.youtube.com/watch?v=u41GEwIq2mE&t=3m59s)

Here's what Erlang looks like:

```erlang
-module(hello).
-export([hello_world/0]).

hello_world() -> io:fwrite("Hello, Partial\n").
```

*If you use Erlang for your projects, Partial is an opportunity to share your experience with others. [Apply](https://goo.gl/qGfmds) for an Erlang talk!*

## [Elixir](https://elixir-lang.org)

Speaking of Erlang and BEAM, there’s no way we can skip Elixir. For the past 6 years it grew in popularity and many developers started experimenting with it.

Elixir was created as a side project by José Valim, a respected Ruby and Rails contributor.

Everything in Elixir is an expression, it features a dynamic dispatch approach to polymorphism and it is really, really fast. Even if you don’t use it for parallel programming, it can handle millions of connections, thanks to its smart design and the use of battle tested Erlang tricks.

Having features like pattern matching, meta programming, lightweight multithreading, developer-friendly syntax and tight integration with OTP, Erlang tooling, Elixir is becoming more and more popular every year.

A simple Hello World in Elixir:

```elixir
IO.puts "Hello Partial!"
```

At Partial, we’ll be covering Elixir with speakers like Saša Jurić (author of “Elxir in action”) and Michał Muskała (core member of the Ecto team).

*If you want to delve deeper and learn more about Elixir – be sure to [get your ticket for Partial](http://partialconf.com/tickets) today!*

## [Elm](http://elm-lang.org)

Developers familiar with React, Redux and other popular web UI technologies will appreciate the simplicity of Elm. It’s a safe and fun way of building interfaces in the browser.

Elm was created by Evan Czaplicki for [his thesis](http://elm-lang.org/assets/papers/concurrent-frp.pdf) in 2012. The compiler is written in Haskell, and it’s a complete package – it has built-in REPL, dependency manager, debugger and libraries to manipulate the DOM. This means that you can quickly start developing your idea, without doing a tedious initial setup.

Each Elm program is separated in three main parts:

- Model - it holds the state of the application
- Update - how you update your state
- View - declarative representation of your interface

Thanks to its focus on immutability and static types it helps developers in creating easy to maintain code and there’s practically no way you can ship broken code due to the strictness of its compiler. Another great thing about Elm is the helpful error reporting – every exception is accompanied with the most important details + documentation and links to online resources.

This is a basic Hello World module in Elm:

```elm
module Hello exposing (..)

import Html exposing (text)

main =
    text "Hello Partial"
```

In September [Ilias Van Peer](https://blog.ilias.xyz) will talk more about Elm at the first edition of Partial.

*If you have a project written in Elm, and you want to present it to other developers – consider [applying for a talk](https://goo.gl/qGfmds) at Partial.*

—

This the end of part 2, next week we’ll cover React and Clojure. For more news about the conference [follow us on Twitter](http://twitter.com/partialconf), or [subscribe to the newsletter](http://partialconf.com/#subscribe).
