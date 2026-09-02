# Replace `org.opentorah.xml` with `org.podval.xml` codecs

Plan to retire the handmade XML parsers and unparsers in this repository
(`ElementTo`, `Parsable`, `Parser` / `ParserState`, `Unparser`) and bind the
same documents with `org.podval.xml.XmlCodec` derived from ZIO Schema.

This is a plan, not an implementation. Gradle wiring so `core` can see the
library is already in place (composite `includeBuild` of site-publisher when
that checkout exists).

Codec mapping rules and modifier names live in the Site Publisher design note
(section **XML codec**). This file is the OpenTorah-side migration.

## Current state

`org.opentorah.xml` is a ZIO state-monad XML layer over `scala.xml.Elem`:

- `From` loads a document (URL, classpath resource, in-memory element). Load
  uses Xerces SAX with XInclude. Nested `xml:base` is then rewritten because of
  [XERCESJ-1102](https://issues.apache.org/jira/browse/XERCESJ-1102).
- `Parser` is `ZIO[Caching & ParserState, Effects.Error, A]`. `ParserState`
  consumes attributes and child nodes in order and fails if leftovers remain.
- `ElementTo` / `ElementsTo` (including `Union` and `choices`) describe which
  element names map to which Scala types. `Unparser` is the reverse.
- `HasName.load` / `bind` wrap a resource whose root name is the catalog
  (`Selector`, `Language`, `Tanach`, …) and whose children are the records.
- `Caching` is Caffeine + ZIO, keyed by URL (and other keys).
- Many `Store` methods (`Stores.resolve`, `Store.getPaths`,
  `NumberedStores.findByName`) return `Parser` even when they do not parse XML;
  they only need an effect type.

Call sites:

| Area | What is parsed |
|---|---|
| `core` metadata | `Name`, `Names`, `Language.Spec`; catalogs `Language.xml` and friends via `Names.Loader` |
| `core` store | `Selector`, `Alias`; catalog `Selector.xml` |
| `core` calendar / astronomy | month, season, special-day, zodiac catalogs through `HasName` / `Names.Loader` |
| `texts` Rambam | `MishnehTorah.xml`, `SeferHamitzvosLessons.xml` (`positive` / `negative` / `named`) |
| `texts` Tanach | books, chapters, parsha, aliyah, maftir, `Haftarah.xml` (customs, `<none>`, variants, parts), `CustomTree.xml`, `ReadingSources.xml`, `SpecialReadings.xml` |
| tests | `XmlTest` (text, attributes, namespaces, XInclude / xml:base); `TestBase.resolve` uses `Parser` + `Caching` for store paths |

Unparsers are incomplete: several `texts` types use `unparser: Unparser[?] = ???`.
Decode-only is enough for catalogs that are never written back.

## Target

- Domain types carry `Schema` + `XmlCodec` (the pattern already used for
  publisher `Selector`, `Entity`, collection `<part>`).
- Load is `org.podval.xml`: parse bytes/string/resource/URL to an `XmlAst`
  element, then `codec.decode`.
- Leftover attributes and children are codec errors (`XmlExtras` when a type
  must keep mixed leftovers). There is no `ParserState` stack.
- `scala.xml.Elem` remains available through `org.podval.xml.ScalaXml`; codecs
  are AST-polymorphic. New code can pin ZIO Blocks XML where convenient.
- `org.opentorah.xml` is deleted when nothing in `core` or `texts` imports it.

`org.podval.xml` does **not** replace store *path* resolution. That stays ZIO
(or a smaller effect) in `org.opentorah.store`.

## Gaps in `org.podval.xml` today

The xml module parses with StAX, writes with Paiges, and derives document
codecs. It is not a drop-in `From` + `Parser`.

### Load

- Parse is string-in (`XmlParser.parseXml`). No classpath-resource or URL
  helper analogous to `From.resource` / `From.url`. *(PR 1 added URL/file/resource.)*
- XInclude is **off** by default: publisher `store`/`collection` treats
  `xi:include/@href` as a page reference, not an inlined document.
  `Tanach.xml` includes the chumash books (`parseCatalog(..., xinclude = true)`).
  `XmlTest` fixtures still go through `From` + Xerces until PR 9.
- Xerces is a `core` implementation dependency solely for that remaining
  `From` path.

### Mapping

Most catalogs are a straight `XmlCodec.derived` once the records are case
classes with `@Modifier.config`. Known mismatches:

1. **`Name`**: decode accepts either attribute `n` or character content, not
   both; unparser always writes `n`. Schema.derived has no “this or that”
   combinator — custom codec (or two optional fields plus a constructor check).
2. **`Names.withDefaultNameParsable`**: parent may have `n="…"` as the default
   name *or* child `<name>` elements. Same idea: optional attribute plus
   `Seq[Name]`, then the existing merge.
3. **Lookups during parse**: `TanachBook.Parsed` calls `HasName.find` inside
   the parser. Codecs are pure. Keep the existing two-phase shape: decode a
   DTO, then `resolve` / `bind` (this already exists for Tanach metadata).
4. **`ElementsTo.Union` / `choices`**: sealed-trait codecs already cover the
   Rambam `positive`/`negative`/`named` case (`XmlCodecSpec` `Lesson`).
   Haftarah’s mix of span, `<part>`, `<custom>`, `<none>`, `<annotation>` on
   one parent is several sibling sequences, not one union — model it as a
   record of `Seq`s, then the current post-processing.
5. **`RawXml` / mixed content**: `XmlExtras` and `XmlNode.Element` (identity)
   replace it. Nothing in current OpenTorah catalogs needs TEI leftovers;
   publisher `Entity` already uses `extras`.
6. **Typed attributes**: `BooleanAttribute` (`yes`/`no`) matches codec
   booleans (`true`/`false`/`yes`/`no`/`1`/`0`). `PositiveIntAttribute` needs
   either a Schema refinement or a check after decode. Prefixed names
   (`xml:id`, `xml:lang`, `xml:base`) work (`@Modifier.config(XmlCodec.Attribute, "xml:id")`).
7. **`ContentType`**: codec leftover rules replace Empty/Elements/Characters/Mixed.

### Caching and ZIO

`org.podval.xml` has no ZIO *runtime* (it uses zio-blocks Schema, not `dev.zio:zio`).
Putting `Caching` into the xml module would add ZIO and Caffeine there.

`Parser` as the store-walk effect is a historical accident. After codecs are
synchronous, `Stores.resolve` should be `ZIO[Caching, Effects.Error, A]` (or
plain `Task` if caching moves to an explicit argument). `Caching` itself
belongs in `org.opentorah.util` (there is already a non-Caffeine `Cache`).

## Key decisions

1. **Codecs in OpenTorah, library in site-publisher.** Domain records stay
   here. `org.podval.xml` stays a Gradle subproject of site-publisher,
   coordinates `org.podval:org.podval.xml`. OpenTorah does not vendor the
   deriver.
2. **Load and XInclude move into `org.podval.xml`; expansion stays opt-in.**
   Default parse does not expand `xi:include` (publisher stores). A separate
   API expands includes and fixes `xml:base` relative to the *initial*
   document so XERCESJ-1102 is not reintroduced. If nothing after collector
   removal still needs expansion, port the `XmlTest` fixtures or drop them
   instead of carrying Xerces forever.
3. **Do not put `Caching` in `org.podval.xml`.** It is an effect environment,
   not an XML binding. Move it to `org.opentorah.util` when `Parser` dies.
4. **Stop using `Parser` for non-XML work** in the same change that deletes
   `ParserState`. Store path resolution is ZIO (plus optional cache), not a
   document combinator.
5. **Decode then bind.** Catalog load is: parse root → decode children →
   `HasName.bind` / per-book `resolve`. No `HasName.find` inside a codec.
6. **`Name` / default `n` get a small custom codec**, not a deriver extension,
   unless a second OpenTorah type needs the same “attribute or text” rule.
7. **Incomplete unparsers stay incomplete** until something writes the
   catalogs. Encode is required where tests round-trip; Haftarah et al. can
   remain decode-only.
8. **Composite build is local-only for now.** `settings.gradle` `includeBuild`s
   `../../Podval/site-publisher` (or `-PsitePublisherDir=…`) when that tree
   exists. GitHub Actions does not check it out; `core` omits the dependency
   so `./gradlew build` still works. After `org.podval.xml` is on Maven
   Central, drop the condition and resolve the artifact in CI.

## Alternatives considered

- **Keep `Parser`/`ParserState`, only swap leaf `ElementTo`s for codecs.**
  Rejected: leftover checking, include URL stacking, and the effect type would
  remain. The goal is to delete `org.opentorah.xml`.
- **Move `org.opentorah.xml` into site-publisher as a second XML stack.**
  Rejected: two parsers. Codecs already exist for the same documents
  (`Selector.xml` is shared).
- **Depend on published xml only, no `includeBuild`.** Blocked until `0.0.1`
  is on Central. Composite substitution is the local development path.
- **Generate the docs site from this Gradle build.** Out of scope; site
  generation stays the existing GitHub Action.

## PR plan

Each PR should be independently reviewable. Site-publisher PRs land first when
the xml module must grow; OpenTorah PRs then use the new APIs.

### PR 1 — Load API in `org.podval.xml` (done)

- **Repo:** site-publisher (`xml/`)
- **Depends on:** nothing
- **Files:** `XmlParser`, `XmlXInclude`, `XmlParserSpec` (fixtures under `xml/src/test/resources`)
- **Work:** parse from string (already), classpath resource, URL, and file.
  XInclude is off by default. `xinclude = true` expands `xi:include` and sets
  `xml:base` relative to the initial document (nested `XmlTest` cases, no
  Xerces). Publisher store parse still uses string parse and does not expand.

### PR 2 — Catalog helper (done)

- **Repo:** site-publisher (`xml/`); publisher `Selector` uses it
- **Depends on:** PR 1
- **Files:** `XmlCodec.decodeChildren` / `decodeCatalog`, `XmlParser.parseCatalog`;
  `Selector.load`
- **Work:** parse a wrapper named `Foo`, decode each child with codec `C`
  (today’s `ElementTo.wrappedSeq(from.name).parse(from)`). Key bind
  (`HasName.load` to enum keys) stays in OpenTorah for PR 3+.

### PR 3 — `core` metadata: `Name`, `Names`, `Language.Spec` (done)

- **Repo:** opentorah.org `core`
- **Depends on:** PR 1–2
- **Files:** `metadata/Name.scala`, `Names.scala`, `Language.scala`;
  `Language.xml`; `MetadataTest`; `HasName.mapByName`; CI checks out
  site-publisher
- **Work:** case classes; custom `Name` codec (`n` xor text); `Names.codec`
  via `decodeChildren`. `Names.Loader` uses `parseCatalog` + `mapByName`
  (calendar / astronomy / texts loaders get this for free). `ElementTo` /
  `Parsable` remain for Selector / Alias / texts until later PRs.

### PR 4 — `core` store: `Selector`, `Alias` (done)

- **Repo:** opentorah.org `core`
- **Depends on:** PR 3 (`Names`)
- **Files:** `store/Selector.scala`, `Alias.scala`; `Selector.xml`;
  `SelectorTest`; `Names.fromDefaultName`
- **Work:** codecs; catalog via `parseCatalog`. `Named` / `HasValues.FindByName`
  stay. `n` on the parent is the default name (same as `withDefaultNameParsable`).
  `ElementTo` dropped; store XML that still used it waits for later PRs.

### PR 5 — Calendar / astronomy catalogs (done with PR 3)

- **Repo:** opentorah.org `core`
- **Depends on:** PR 3
- **Files:** `Names.Loader` users under `calendar/` and `astronomy/` and their
  `*.xml` resources
- **Work:** none beyond PR 3’s generic loader.

### PR 6 — `texts` Rambam and simple catalogs (done)

- **Repo:** opentorah.org `texts`
- **Depends on:** PRs 3–4
- **Files:** `rambam/MishnehTorah.scala`, `SeferHamitzvosLessons.scala`;
  `tanach/Custom.scala`, `ReadingSources.scala`; matching XML
- **Work:** `parseCatalog` + codecs. Lesson `Part` is a tagged union
  (`positive` / `negative` / `named`); `<negative>` now decodes as `Negative`
  (the old `ElementTo[Positive]` for both numbered cases was wrong).
  Decode-only.

### PR 7 — `texts` Tanach books, parsha, Torah spans (done)

- **Repo:** opentorah.org `texts`; site-publisher `parseCatalog(..., xinclude)`
- **Depends on:** PR 6
- **Files:** `TanachBook.scala`, `Chapters.scala`, `Chapter.scala`,
  `Parsha.scala`, `PsalmsBook.scala`, `SpanParsed.scala`,
  `VerseParsed.scala`, `WithNumber.scala`, `XmlDecode.scala`;
  `HasName.findByNames`
- **Work:** DTO decode + existing `resolve` pass. `Tanach.xml` loads with
  `parseCatalog(..., xinclude = true)` so chumash `xi:include`s inline.
  Keep lazy vals that avoid ZIO initialization deadlocks
  ([zio#1841](https://github.com/zio/zio/issues/1841)). `Torah.torahParsable` /
  `spanParser` stay `ElementTo` for PR 8 (Haftarah / SpecialReadings).

### PR 8 — Haftarah and special readings (done)

- **Repo:** opentorah.org `texts`
- **Depends on:** PR 7
- **Files:** `Haftarah.scala`, `SpecialReadings.scala`, `Torah.scala`,
  `WithBookSpans.scala`; tests `ReadsNothingTest`, `SpecialReadingsDataTest`
- **Work:** `<haftarah>` / `<week>` decode as sibling sequences (`part`,
  `custom`, `none`, `annotation`) plus span attributes; `withAnnotations`
  is a function on the DTO, not `ParserState`. `sources` / `comment` /
  `variant` / `precedenceWhenCombined` / `<none>` kept. Torah and maftir
  decode for SpecialReadings.xml. `HasName.mapByName` still binds weeks
  to parshiyos.

### PR 9 — Delete `org.opentorah.xml`

- **Repo:** opentorah.org `core` (and leftover `texts` imports)
- **Depends on:** PRs 3–8
- **Files:** everything under `core/src/main/scala/org/opentorah/xml/`;
  `XmlTest.scala`; `core/build.gradle` (`xerces` if unused);
  `store/*.scala` and `TestBase` still using `Parser`
- **Work:**
  - `Stores.resolve` / `getPaths` / `findByName` return ZIO (with `Caching`
    in `org.opentorah.util` or as a method argument).
  - Move XInclude tests to site-publisher (if expansion exists) or delete
    collector-style fixtures.
  - Drop `scala-xml` as a direct `core` `api` if all AST use goes through
    `org.podval.xml` (it may remain transitive).
  - After xml is on Central, make the `core` dependency unconditional and
    teach CI to resolve it (or keep `includeBuild` for local substitution).

## What not to do in the same PRs

- Do not generate the OpenTorah site from `./gradlew build`.
- Do not fold publisher TEI harvest (`Entity`, `entityLists`) into this
  migration; that stack already uses codecs.
- Do not commit a change to site-publisher `generate()`’s hardcoded path.
- Do not rewrite catalog XML unless a codec cannot express the current shape
  (`Name`’s dual form is the one expected exception, and it stays compatible).
