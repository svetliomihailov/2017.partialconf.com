---
title: Saša Jurić Interview
date: 2017-07-17 14:00 UTC
tags: speaker
---

A little while ago we asked [Saša Jurić][Sasa] a few questions about his
upcoming talk: **Basic guarantees of Erlang virtual machine (BEAM)** at
[Partial]. Here is the interview in a raw, unedited form.

## Tell us a bit about yourself?

My name is Saša, and I've been writing code for some 25 years (15 of which as a
full time professional). My current professional focus is on developing backend
systems using Elixir/Erlang. Previously I've used various other
languages/technologies, such as C++, C#, Ruby, JavaScript. Although I'm now
mostly doing FP with Elixir/Erlang, for the most of my programming life I've
actually used OOP.

## How did you get into programming?

My parents bought our first computer way back in 1991. I wanted a computer for
a long time, but to my disappointment the one they bought was not a gaming
computer, but a PC machine which was mostly useful for office type of work. A
lucky coincidence was that a friend of mine also owned a PC, and he was already
into programming. So he showed me some basics of BASIC and I started writing
some simple programs. It was love at first sight, and I immediately knew that
this is what I wanted to do for the rest of my life. Some 25 years later I
still feel the same :-)

## How did you get into functional programming?

Back in 2010, we needed to implement push notifications for an online betting
system, and at that time Erlang was one of the few available technologies which
made that job easy. So I really wasn't looking for a functional language. I
reached for Erlang because we needed concurrency and scalability, and learning
FP was a necessary consequence. It took a while to adapt to that mindset, but
now I'm definitely a fan of FP, and prefer it to OOP.


## Why that tech?

For me, the best thing about Elixir is Erlang :-) It is a fantastic piece of
technology, with a very strong focus on the challenges inherent to practically
every backend system. Couple that with a 20+ years of constant development, and
you get a very unique piece of technology. I'm not aware of anything else that
has such long tradition and is built from the ground up with such concrete
goals in mind.

That being said, Elixir definitely brings a lot of useful things to the table.
By aiming to improve developer productivity Elixir simplifies our job of
reaping all the great benefits of Erlang. And in my opinion it definitely
succeeds in that goal! I've worked with plain Erlang for 7 years now, and with
Elixir for about 4, and I definitely prefer Elixir/Erlang combination over
plain Erlang.

Not everyone will agree with that, and that's fine, but for me I think they
complement each other perfectly. Erlang, in particular the BEAM virtual machine
and the OTP framework, revolves around scalability, fault-tolerance,
distributed systems, and ultimately high-availability. Elixir revolves around
features in the language and tools around the language to simplify our
day-to-day job. With such focus, I think that Elixir is not a "different
Erlang", but more an Erlang with the missing pieces.

So to summarize, as a mere user of Elixir and Erlang, I have a huge respect for
both technologies because they have a very strong and particular focus, a sense
of purpose if you will, which in my opinion is a quite rare trait these days.

## Why that talk?

In this talk I'm trying to cut to the core of what makes Elixir/Erlang special,
compared to other technologies out there.

There are some fundamental differences between Elixir/Erlang and most other
technologies, including those which are occasionally mentioned as "Erlang
alternatives". These differences can have a large impact on your system in
production, and on your whole development process. However, the problem is that
you don't usually see these differences at a casual glance.

I certainly wasn't aware of that when I reached for Erlang, but I was lucky
that at the time there were not so many mature competitors in that particular
space. Today the story is different, and with more choices of mature languages
and technologies, it's not immediately clear when should Elixir/Erlang be
chosen over X (or vice versa). I have the feeling, that people frequently pick
their technology stack based on some superficial factors, such as language
syntax or shallow synthetic benchmark comparisons, TIOBE index, or comments
made on Hacker News.

With this talk, I'm trying to make the audience aware of these, not so obvious,
but very important differences. After the talk they should have a much better
grasp on what makes Elixir/Erlang special, and therefore should be able to make
a more educated guess when choosing the technology for their future projects.

—

We'd like to thank [Saša][Sasa], for taking the time to do the interview.  We
hope you liked it as much as we did! Keep an eye on the blog as we'll be
posting more interviews in the following weeks. Also make sure to get your
[Partial] ticket, so you can see [Saša][Sasa] live. ❤️

[Partial]: http://partialconf.com
[Sasa]: http://theerlangelist.com
