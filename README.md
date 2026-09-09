# Digital Judaica Done Right :)

TODO convert READMEs to AsciiDoc and move them into /docs

![](https://github.com/opentorah/opentorah/workflows/CI/badge.svg)

[Writings](http://www.opentorah.org) on the subject.


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
to be deployed separately, as a website (docs), or a service (texts).

If users that need just the calendar code appear, I'll think about splitting that ;)
