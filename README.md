# Digital Judaica Done Right :)

TODO convert READMEs to AsciiDoc and move them into /docs

![](https://github.com/opentorah/opentorah/workflows/CI/badge.svg)

[Writings](http://www.opentorah.org) on the subject.

## XML processing ##

### XML parsing ###

I use [Scala XML](https://github.com/scala/scala-xml) support to parse XML documents,
and then parse resulting in-memory structures into domain-specific objects.
I found that doing it by hand results in code that is error-prone, fragile, and not very readable,
since one needs to keep track of which nodes were already consumed and which still remain for the
continuation of the parsing.

I decided to switch to a model that uses a state monad of sorts.
I did not immediately realize that ZIO actually has state monad built-in, so I rewrote my parsers
using Cats; that was a major improvement :) When I finally understood how to use ZIO's state monad,
I rewrote the code again using ZIO - what a pleasure!

### XML generation ###

In web-apps, I need to generate XML. I tried using [ScalaTags](https://com-lihaoyi.github.io/scalatags/) for that,
and they work, but there is no clean [interoperability](https://github.com/lihaoyi/scalatags/issues/102) with Scala XML,
so generating - say - HTML wrappers for XML parsed from files is not pleasant, and since ScalaTags does not (in my
opinion) have obvious advantages over Scala XML, I decided not to bother with it.

### XML pretty-printing ###      

Scala XML does provide a pretty-printer, but it doesn't do what I need it to do, especially when formatting
TEI documents. For instance, it inserts a break between `</e1>` and `<e2>` in `<e1>...</e1><e2>...</e2>` and
between `</e1>` and `text` in `<e1>...</e1>text`, thus separating notes from what they are notes on and
punctuation from its content by spaces. Attempts to modify the behavior of the Scala XML pretty-printer
(or at least to fix what looks like an obvious bug in its `Print.merge`) were not successful,
so I started looking for alternatives.

[Compact, Streaming Pretty-Printing of Hierarchical Data](https://www.lihaoyi.com/post/CompactStreamingPrettyPrintingofHierarchicalData.html)
looked promising, but it looks like to fine-tune behavior of the pretty-printer, I'd have to rework the code each time:
as with ScalaTags, [Li Haoyi's](https://www.lihaoyi.com) focus seems to be performance, but I need flexibility to get
the format I want without rewriting the whole algorithm for each experiment...

I remembered a paper by [Phil Wadler](http://homepages.inf.ed.ac.uk/wadler/) on  pretty-printing:
[A prettier printer](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf),
and almost started implementing it in Scala, but it turns out that there is an implementation already:
[Typelevel Paiges](https://github.com/typelevel/paiges). With its declarative approach, I was able to
tweak the output to my liking :)


## Code Structure ##

### Monorepo ###

Inspired by [Advantages of monorepos](https://danluu.com/monorepo/) and
[Unorthodocs: Abandon your DVCS and Return to Sanity](https://www.bitquabit.com/post/unorthodocs-abandon-your-dvcs-and-return-to-sanity/)
(what a gem!), I switched to using monorepo for the opentorah.org projects
(with the number and sizes of projects, I think I am safe from the issues that
Google and FaceBook experienced ;)).

One never knows when there will arise a need to split or merge repositories,
so this is how I did it:

To extract directories from a repository into a separate one:

```shell
  $ git filter-repo --path <path1> --path <path2> ...
```

Since `filter-repo` does not try to preserve history for the files that were
[renamed](https://github.com/newren/git-filter-repo/issues/25), before
extracting the directories, one should figure out what other directories
files in them previously resided in. Looking through the output of
`$ git log` is one way; another is to look at the renames report that
`$ git filter-repo --analyze` generates.

To merge repository `old` into repository `new` preserving history (one hopes!):

```shell
  $ cd <new>
  $ git remote add -f old <old>
  $ git merge ald/master --allow-unrelated-histories
```

Since it is impossible to have a file in Git where only the last revision is kept
but revision history is automatically discarded, and for the generated files
(like HTML, PDF and EPUB of the papers) to be visible on the site they need to be checked in,
I might end up pruning their history periodically using `$ git filter-repo`...

### Modules ###

Historically, thematically cohesive packages were relegated to separate Gradle modules, since they originated in
separate repositories. This approach helps enforce layered architecture: no imports of the higher layer types in the
lower layers. It also helps to trim down unneeded dependencies when using specific subset of the functionality.

Since I am just about the only user of the code, the latter reason is not compelling.

The first reason is not that compelling either: Gradle doesn't block cyclical inter-module dependencies completely.
Besides, relying on Gradle in this respect means putting every cohesive package in a separate module, which seems excessive.

As a result, I currently use the fewest number of modules approach: code is separated in a module only if it needs
to be deployed separately, as a website (docs), or a service (collector, texts).

If users that need just the calendar code appear, I'll think about splitting that ;)
