# XML processing #

This module
- hosts XML-related code used by other sub-projects
- provides common utility methods for working with files, collections, caching, Gradle etc.


## XML parsing ##

I use [Scala XML](http://homepages.inf.ed.ac.uk/wadler/) support to parse XML documents,
and then parse resulting in-memory structures into domain-specific objects.
I found that doing it by hand results in code that is error-prone, fragile, and not very readable,
since one needs to keep track of which nodes were already consumed and which still remain for the
continuation of the parsing.

I decided to switch to a model that uses a state monad of sorts.
I did not immediately realize that ZIO actually has state monad built-in, so I rewrote my parsers
using Cats; that was a major improvement :) When I finally understood how to use ZIO's state monad,
I rewrote the code again uzing ZIO - what a pleasure!

## XML generation ##

In web-apps, I need to generate XML. I tried using [ScalaTags](https://www.lihaoyi.com/scalatags/) for that,
and they work, but there is no clean [interoperability](https://github.com/lihaoyi/scalatags/issues/102) with Scala XML,
so generating - say - HTML wrappers for XML parsed from files is not pleasant, and since ScalaTags does not (in my
opinion) have obvious advantages over Scala XML, I decided not to bother with it.

## XML pretty-printing ##      

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
