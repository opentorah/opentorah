# Digital Judaica Done Right :)

![](https://github.com/opentorah/opentorah/workflows/CI/badge.svg)

[Writings](http://www.opentorah.org) on the subject.

Scala 3.9, Java 25, Gradle 9. Version **0.11.0**.
Libraries `org.opentorah:opentorah-core` and `org.opentorah:opentorah-texts` publish to Maven Central.

```shell
./gradlew build
./gradlew :docs:generateSite
./gradlew :docs:serveSite
```

`:docs:generateSite` runs `generateTables` first and writes `docs/_site` (GitHub Pages).
Plugin id `org.podval.tools.site-publisher`. A local site-publisher checkout at
`../../Podval/site-publisher` is used when present (`pluginManagement { includeBuild }` plus
settings-body `includeBuild`, `-PsitePublisherDir=`); CI resolves the plugin and
`org.podval.tools:org.podval.tools.publisher` from Maven Central.


## What's here ##

Three Gradle modules:

- **core** (`opentorah-core`) — Rambam's arithmetic and astronomical calendar, mixed-radix numbers, angles.
  See [README-calendar.md](README-calendar.md).
- **texts** (`opentorah-texts`) — Tanach structure, rites (`Custom`), Torah and haftarah readings,
  Rambam learning schedules. Tanach, Mishneh Torah and Sefer HaMitzvos are Stores.
- **docs** — papers (calendar, dream, typesetting) and the [www.opentorah.org](http://www.opentorah.org) site,
  built with [Podval Site Publisher](https://github.com/dubinsky/site-publisher) and deployed by GitHub Actions.

XML, names/metadata and Store live in [`org.podval.xml`](https://github.com/dubinsky/xml)
(`org.podval:org.podval.xml`). A local checkout at `../../Podval/xml` (or `-PxmlDir=…`) is picked up
as a Gradle composite build when present; otherwise the published artifact is used.

Related sites, not built from this repository:

- [www.chumashquestions.org](https://www.chumashquestions.org) — Rabbi Wichnin's Chumash Questions;
- [www.alter-rebbe.org](https://www.alter-rebbe.org) — archive of early Chabad documents.


## No longer here ##

Removed in 0.11.0 (August 2026):

- **Collector** — used to generate and serve www.alter-rebbe.org.
  Old notes: [README-collector.md](README-collector.md).
- **Calendar web application** — use [hebrewcalendar.net](https://hebrewcalendar.net).
- In-repo XML stack, ZIO runtime, DocBook plugin, Jekyll.

Historical toolchain notes: [README-docbook.md](README-docbook.md),
[README-asciidoctor.adoc](README-asciidoctor.adoc).

See [CHANGELOG.md](CHANGELOG.md) for the rest.


## Modules ##

Historically, thematically cohesive packages were separate Gradle modules — they started as separate
repositories. That helps enforce layering and trims unused dependencies, but I am just about the only
user of the code, Gradle does not fully block cycles anyway, and a module per package is excessive.

A module is split out only when it has to ship on its own: `docs` as the website; `core` and `texts`
as published libraries. Calendar stays in `core` until someone needs it without the rest.
