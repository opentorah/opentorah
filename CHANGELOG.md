# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.10.8] - 2023-
- build: dogfooding ScalaJS plugin for running tests
- build: switched papers to AsciiDoc
- build: removed DocBooks processing from `docs`
- build: removed mdoc from `docs`
- build: removed SASS from `docs`
- site: removed DocBook processing, math and highlighter support
- site: moved site and tei packages from the core module into the collector module
- site: ToHtml became TeiToHtml
- docbook: removed ToHtml and direct (non-XSLT) processing functionality and Playwright
- docbook: moved docbook, fop, math and pdf packages from the core module into the docbook module
- html: moved Html and A into the xml package, and merged the rest of the package into the site package in the collector module

## [0.10.7] - 2023-01-28
- fix: set the fontSize attribute even if its value is the default
- fix: escape backslashes in TeX strings
- cleanup: XML Attribute;
- cleanup: math Input;
- exactification;
- cleanup: Numbers;
- Numbers.fromString();
- cleanup: astronomy;

## [0.10.6] - 2023-01-18
- dependency updates;
- move from zhttp to zio-http;
- cleanup: Html, ToHtml, DocBook, Tei;
- DocBook direct;
- calendar paper;
- LeapYearsTest (from Pirush);
- superscripted d,h,p and m in the TimesDigit;
- fix: npm install;
- J2V8 support removed;
- JEuclid support removed;
- cleanup: MathJax;
- switched to MathJax 3;

## [0.10.5] - 2022-09-15
- dependency updates;

## [0.10.4] - 2022-07-31
- Gradle cleanup;
- dependency updates;

## [0.10.3] - 2022-07-05
- Gradle cleanup;
- dependency manipulations and other utilities from the ScalaJS plugin moved here;
- dependency updates;
- ZIO 2.0!

## [0.10.2] - 2022-06-16
- update Gradle, Zinc, ZIO, ZHTTP and other dependencies;
- minor ZIO cleanup;
- minor code inspections cleanup;
- ServiceApp cleanup;
- re-added the `util` module;
- `util` module cleanup;
- Node support in the util module;
- Gradle utility class with extension methods;

## [0.10.1] - 2022-04-20
- FOP 2.7;
- Saxon 11;
- dependencies update;
- using scala-xml with my enhancements;
- make XML includes leave xml:base traces;
calendar: modern sun position calculations using com.kosherjava:zmanim;
calendar: conversions to java.time.LocalDateTime and Julian Day;
docbook: installDocBookDependencies task;
site: list DocBook files to pretty-print;
site: use per-entity reference lists instead of the monolithic one to avoid OOM;

## [0.10.0] - 2022-01-20
- from now on, version applies to DocBook plugin too;
- Scala 3: new syntax and strict equality;
- switched from http4s to zhttp;
- ZIO 2.0;
- merged "schedule" module into "texts";
- merged "tei", "site" and "calendar" modules into "base" and renamed it "core";
- consolidated all papers (and data generators) and mdoc in the "docs" module;
- ScalaTest 3.2.10 uses current FlexMark and there is no longer a conflict with mdoc; removed build classpath trickery;
- ToHtml cleanup: levels of notes;
- hand-coded replacement for Gradle/Ant Zip and Copy (with filtering);
docbook: *breaking changes from the previous release*
- moved DocBook stuff that is not the plugin itself into core;
- reworked plugin's the Gradle DSL;
- introduce configuration by XML files;
- removed "docBook.outputFormats" environment variable;
- ProcessDocBookTask self-configures from the extension - no other instances can be used;
- output and temporary directories are per document even if there is only one document configured;
- per-document configuration;
- to support per-variant configuration of the versions, math etc., generate main stylesheet and XML catalog for each variant;
- variant name is used for the output only if there are more than one variant of the same format for the document;
- generate under `build`, not `src`: stylesheets, substitutions DTD, XML catalog, FOP configuration file;
- generate default customization stylesheets (under `build`) only if they are used *and* non-empty;
- do not generate input document blanks or CSS file if they do not exist;
- include customization stylesheets and custom XML catalog in processing only if they exist;
- working on DocBook direct;
- SiteService/Site are processing DocBook without the plugin.

## [0.9.0 docbook] - 2021-11-03
- Scala 3;
 
## [0.4.0] - 2021-11-03
- from now on, version applies to calendar too;
- Scala 3;
- XML parsing and processing cleanup;
- abstracted XML parsing over ScalaXml/DOM;
- From and Parsing abstracted over Xml;
- Store/Site/Collector cleanup;
- Tanach cleaned up and Storified;
- Store Alias and Path walking;
- no unsafeRun() in Site/Collector;
- merged HtmlContent into Store;
- codified pre-title Store header;
- moved "store" package into the "site" module;
- merged "schedule" module into "texts";
- merged "tei" module into "site".

## [0.2.6; 0.3.7 calendar] - 2021-08-17
- consolidated 'util', 'xml' and 'metadata' modules into 'base';
- moved Store, By, Selector, Directory, WithSource and some Site/HtmlContent methods into the
    'store' package of the 'base' module;
- consolidated 'calendar-paper' into 'calendar';
- consolidated 'fop' into 'docbook';
- consolidated 'calendar-service' into 'schedule';  
- consolidated 'dream' and 'typesetting' papers into 'texts';
- added 'site' module with Site/HtmlTheme/HtmlContent/Viewer and Markdown support;
- added 'docs' module;
- added theme files to the project's site;
- added SASS -> CSS compilation to the project's site;
- fixed a bug with the first day of the Jewish epoch (molad BaHaRaD) having number 2 instead of 1;
- fixed dates printing bug;
- added Julian calendar;
- cleaned up `Numbers` and `Calendar`;
- allow allowing `RawXml` elements to have attributes;
- pretty-printing in Site;
- collector and DocBook plugin use the 'site' module;
- support for MathJax3;
- code highlighting using `highlightjs`;
- PrettyPrinter encodes XML specials in attributes, character data and preformatted blocks;
- `DoubleEscapeHtmlNodeExtension` removed;
- consolidated Site commands (including siteUpload) into SiteService;
- working on direct DocBook -> PDF transformation (without plugin);
- working on the DocBook stylesheet and element transform;
- experimenting with the PDF generation with Playwright;
- correct `jib.from.image`;
- centralized dependency versions using Gradle 7 features;
- dependency updates;
- code adjusted to compile with Scala 3 (using 
  [patched Gradle](https://github.com/gradle/gradle/pull/18001) and Scala 3.0.3 nightlies);

## [0.8.3 docbook] - 2021-08-17
- **note:** `mathJax.isEnabled` renamed `mathJax.enabled`;
- **note:** `isJEuclidEnabled` renamed `jEuclidEnabled`;
- support for MathJax3;
- merged FOP plugin back into this module: nobody used it ;)
- use the 'site' module;
- `DoubleEscapeHtmlNodeExtension` removed;

## [0.2.5] - 2021-02-28
- site is now dynamic!
- double-escape XML tag start characters and such in Markdown code blocks;
- PrettyPrinter supports preformatted elements;
- and doesn't let Paiges to mis-indent them;
- ZIOfication;

## [0.2.4] - 2021-02-04
- moved from JCenter/Bintray to Maven Central (see http://dub.podval.org/2021/02/04/bintray-is-dead.html)!
- no tests in master CI
- no artifact uploads from GitHub Actions

## [0.2.3] - 2021-02-02
- serve using local Docker
- facsimiles - hierarchically

## [0.2.1] - 2021-01-28
- 'collector' re-written
- use correct thread-pools to avoid ZIO deadlocks with nested `unsafeRun()` calls in CPU-constrained situation;
- use the latest Cloud Run Gradle plugin to run in the local Docker;

## [0.2.0] - 2021-01-27
My attempt at re-using Store and friends for texts other than the alter-rebbe archive failed:
it was not general enough, and too inflexible to be used even in its original context.
Code in the 'collector' module was re-written, and 'store' module emptied out.
- Service class enhanced to serve dynamic content;
- Main and Cutter moved into the alter-rebbe.org repository;
- site XML files there adjusted to the new format;

## [0.1.67] - 2021-01-15
- Attribute.Parsable and ensuing cleanup;
- everything is a Parsable now ;)

## [0.3.4 calendar] - 2021-01-15
- preparing for the switch to Scala 2.13/3;
- reworked family polymorphism to not use general type projections
  (they go away in Scala 3), so we are back to all family members being in one file :(

## [0.1.66] - 2021-01-14
- Scala 2.13+
- split-file family polymorphism encoding in 'calendar' removed :(

## [0.1.62] - 2020-12-20
- xml.Element extends both FromXml and ToXml;
- calendar not using store;
- hybrid store/collector new generation;
- selectors in metadata, not in the store file;

## [0.1.61] - 2020-12-14
- ToXml and Parsable cleanup;
- replaced Parsable.withInclude() with unlimited redirects via Element.canRedirect;
- From.url(..., isRedirect) to mark redirects in Context;
- Context.fromUrls; Store parsers do not have to keep track of the URLs (but still do);

## [0.1.60] - 2020-12-13
- synchronizing local directory to a Google Storage bucket;
- use CRS32c to detect changes in uploaded files;
- introduced `.gsignore` file to exclude files from the Google Store sync;
- consolidated build functionality (verify/generate/upload/serve site) in the Main object;
- writing and reading References;
- made PrettyPrinter.chunkify() tail-recursive;

## [0.1.56] - 2020-12-06
- converting TEI to HTML in code (no more CETEIcean);
- generating HTML in code (no more Jekyll for the TEI files);
- generating HTML from Markdown files (no more Jekyll for them);
- generating CSS from SCSS at build time (no more Jekyll!);
- folded `collector-service` into `collector`;
- serving local files without Jekyll (which is no longer used for anything);
- UI improvements;
- keep one instance of the service always warm (no more Cloud Scheduler);
- using CloudRun Gradle plugin;
- XML Namespace and Dialect cleanup;
- XML Attribute and Antiparser cleanup;

## [0.1.53] - 2020-08-23
- texts metadata parsers ZIOfied a little
- PrettyPrinter chunking cleanup
- gathered standard XML namespaces in the Namespace object
- abstracted pretty-printer over XML representation
- moved TEI support into the `tei` module
- introduced `Antiparser` AND USED IT THROUGHOUT
- switched to Logback logging;
- added blog and moved posts from `alter-rebbe.org` here;
- added Bintray upload to the `CI` GitHub action
- added JIB image upload and Cloud Run Deploy to the `CI` GitHub action
- tracking StaticFile fix;

## [0.7.53 docbook] - 2020-08-23
- use XML literals instead of strings
- pretty-printing DOM

## [0.1.52] - 2020-07-14
- added PrettyPrinter.alwaysStackElements
- merged Attribute and AttributeX
- Attribute defaults and conversions cleanup
- XML parsing cleanup

## [0.7.52 docbook] - 2020-07-14
- parameters cleanup
- using Xerces for loading Sla XML

## [0.1.52] - 2020-07-03 (`collector-service`)
- instructions for the `collector-service` setup;
- logging using Logback to integrate with Cloud Run logging;
- use structured logging (with request trace);
- log environment variables retrieval;
- log files retrieved from the documents site (with timing);
- keep the service warm using Cloud Scheduler;

## [0.1.51] - 2020-06-26
- fixed ZIO deadlock in the Rambam schedules
- deleted `importers` code but kept the notes: `importers.txt`
- XML processing cleanup

## [0.7.51 docbook] - 2020-06-26
- added output format `variants` (like `pdf-a4`)
- Layout cleanup

## [0.1.50] - 2020-06-01
- added README.md
- clean up SpecialDay
- clean up Haftarah

## [0.3.3 calendar] - 2020-05-31
- simplified directory structure
- moved common code to appropriate modules
- moved Tanach and Rambam structures and support into the `texts` module
- moved schedule-related stuff into the `schedule` module
- cleaned up SpecialDay (`schedule`)
- cleaned up and updated some dependencies
- removed ScalaTags (`calendar-service`)

## [0.7.50 docbook] - 2020-05-31
- caching frameworks for DocBook processing
- cleaned up data generation
- cleaned up generated catalog etc.
- updated FOP to 2.5
- updated to Saxon 10 and used it for FOP

## [0.1.49] - 2020-05-08
- split out `metadata` module
- split out `texts` module

## [0.7.49 docbook] - 2020-05-08
- merged into the `opentorah` monorepo
- moved packages to `org.opentorah`
- moved common code into appropriate modules

## [] 2020-04-28
- centralized module setup (scala, library, service, mdoc, paper Gradle scripts)

## [0.3.1 calendar] - 2020-04-26
- calendar: packages renamed to org.opentorah
- split out jewish-calendar.org website
- moved www.opentorah.org into a separate repository
- merged everything into the `opentorah` monorepo
- converted service to ZIO
- 'xml' module
- created `collector` and `collector-service` modules

## [] - 2020-04-21
- pretty-print Stores read from files back into them
- site hierarchy
- introduce Site class
- write site in pull mode
- process pb elements
- reports in TEI
- collector service

## [] - 2020-04-04
- pretty-printing Stores read from files
- FromFile/Inline
- Urls

## [0.1.29] - 2020-03-17
- ToXML
- Parsable: name-to-parser made explicit
- UnionParsable
- Choice
- metadata with include
- Selector, Binding, Path

## [] - 2020-03-09
- source TEI documents processed into TEI documents for display
- collector: moved packages to org.opentorah
- allow and report ref-less names
- report misnamed nameds

## [0.1.22] - 2020-03-08
- XML pretty-printing using Paiges
- TEI support enhanced
- parsing moved from Cats to ZIO
- Parser error trace
- XML support enhancements
- moved packages to org.opentorah

## [0.1.7] - 2020-02-19
- metadata, XML and utility functionality from the 'calendar' project moved to the 'store' module
- XML support from the 'archive' project moved to the 'store' module
- XML support consolidation
- monadic parsing using Cats
- deleted the previous attempt at the 'store' functionality
- Store
- TEI support from the archive project moved here

## [] - 2019-12-08
- facsimiles hosted externally
- facsimile checks disabled
- 'role' attribute on Named and lists

## [0.7.14 docbook] - 2019-12-02
- split FOP plugin into a separate module

## [0.7.13 docbook] - 2019-11-26
- force use of Xerces by Saxon always, including SVG

## [0.7.12 docbook] - 2019-11-26
- force use of Xerces by Saxon

## [0.7.11 docbook] - 2019-11-25
- update Gradle to 6.0.1; call metadataSources.artifact() explicitly to handle NodeJS
  repository (was done automatically in Gradle <6)
- update FOP to 2.4; exclude new JAI dependencies that rely on JBoss repository
  add equivalent dependencies from JCenter

## [] - 2019-11-13
- addressee
- navigation cleanup

## [] - 2019-09-23
- multiple collections
- CC BY 4.0 license
- `collector` module
- Layout enhancements
- manuscript and book page numberings
- Name/Names/Entity/TEI to and from XML
- no more '#' in name references

## [0.7.10 docbook] - 2019-09-15
- MathJax ImagePreloader should refuse to handle non-DOMSource images

## [0.7.9 docbook] - 2019-09-15
- switched from Travis to GitHub Actions

## [0.7.8 docbook] - 2019-07-04
- MathML configuration for HTML
- MathJax ex size
- parsing errors logging
- dependencies and up-to-date determination for the `processDocBook` task

## [0.7.7 docbook] - 2019-06-26 - MathJax for PDF!
- wrap TeX and AsciiMath using an XML filter
- Node support (inspired by the Gradle Node plugin)
- install `mathjax-node` Node module and dependencies
- determine display mode from DocBook context (including for MathML)
- introduced Namespace
- typeset math running Node in an external process
- convert SVG sizes mathjax produces to those that FOP understands
- retrieve, extract and load platform-specific J2V8 library for native Node bindings
- MathJax configuration DSL

## [] - 2019-04-23
- CETEIcean behaviours for pb etc.
- indexes in TEI
- navigation links

## [] - 2019-03-25
collector:
- switched to Gradle
- switched to Git
- TEI 'ref' attribute converted to hyperlinks
- transcriber in the TEI header
- switched to CETEIcean

## [0.7.0 docbook] - 2019-03-10
- cleanup

## [0.6.9 docbook] - 2019-02-24
- multiple documents support
- per-section defaults and default customization files
- intercept Saxon warnings and log them as info
- removed local copy of the JEuclid FOP plugin
- substitutions for PDF metadata

## [0.6.8 docbook] - 2019-02-21
- renamed `documentName` to `document`, `cssFileName` to `cssFile`
- `use.id.as.filename` by default
- added hyphenation rules dependency
- suppressed "font directory" warning (set baseUri)
- chunk quietly and fast
- lifecycle and warning logging with logging ErrorListener
- skip "main" output file when chunking in XSLT 1.0 - and supply a writer in XSLT 2.0
- cleanup

## [0.6.7 docbook] - 2019-02-20
- Layout/DocBook2 cleanup
- namespace-aware DocBook XSLT 1.0 stylesheets

## [0.6.6 docbook] - 2019-02-20
- merged XSL customizations, temporary directory, output directory and
  output formats configuration for XSLT 1.0 and 2.0 stylesheets
- versions of the XSLT stylesheets are now configurable

## [0.6.5 docbook] - 2019-02-19
- XSLT files and XML catalog customizable and available to Oxygen
- per-format parameters
- HTML with XSLT 2.0 stylesheets

## [0.6.0 docbook] - 2019-02-17
- resolving is delegated via generated XML catalog (and substitutions DTD) that can be used in Oxygen

## [0.3.0 calendar] - 2019-02-13
- Psalms for the day of the week, year's molad and delay
- website index.html consolidation
- use sync for publishing the paper into docs
- using DocBook Plugin!
- using JEuclid: MathML in PDF!
- font that has all required scripts
- split build.gradle (and removed POM)

## [0.5.0 docbook] - 2019-02-13
- cleanup and tests
- using caching resolver from org.xmlresolver
- MathML in PDF via JEuclid FOP plugin

## [0.2.2 docbook] - 2019-02-10
- XSLT 2.0 DocBook stylesheets with Saxon 9 HE

## [0.2.1 docbook] - 2019-02-06
- EPUB3
- EPUB font embedding

## [0.2.0 docbook] - 2019-02-05
- configurable output formats
- better substitutions
- cleanup
- README.md
- publishing to plugin.gradle.org

## [] - 2019-01-25 - 01-31
docbook:
- integration with Gradle Task Input/Output
- substitution of entities
- evaluation of processing instructions

## [0.2.9 calendar] - 2019-01-23
- calendar: using Cats and Algebra for testing the laws
- optimization: Years are cached; Month/Day get their Year/Month at construction if it is known
- merged jewish-calendar.org site into this repository
- configuring Jekyll (and MathJax) for the project's site
- more documentation
- moved DocBook plugin into a separate repository
- added to schedule: Sefer Hamitzvos, Molad, Shabbos Mevarchim, seasons, year type and cycles
- normal forms of numbers simplified and normalization centralized
- exactification
- cleanup

## [] - 2019-01-09
- docbook: moved it into a separate repository

## [0.2.5 calendar] - 2018-12-31
- structure of the Rambam's Mishneh Torah and mapping of the lessons to Sefer Hamitzvos
- Rambam learning schedules
- Psalms schedule
- CHANGELOG.md

## [0.2.0 calendar] - 2018-12-16
- calendar: simplified POM configuration and Bintray publishing
- built and uploaded custom Google Cloud Build Gradle builder that supports Gradle 5.0
- credits and sources
- "caches"
- annotate readings with their sources (Shabbos, special day, etc.)
- chapters for the Writings books and day of the month/week divisions for the Book of Psalms
- code reorganization

## [] - 2018-12-06
- Calendar service using http4s with Scalatags
- deploy the service as a Docker image via pull-triggered Google CloudBuild that resstarts the VM
- multi-lingual bi-directional UI
- better Custom minimization
- Chumash Chitas

## [] - 2018-08-03 - 2018-11-14
- schedule-related data, including some quotes from Shulchan Aruch
- Chumash book structures and metadata support from the "Digital Judaica" project (Names, Name, Language, Numbered)
- Tanach and Haftarot structure
- special days of the Jewish calendar and their effect on the Torah reading
- parsing and binding metadata
- introduced XML utilities (based on the ones from the Store project?)
- Chapters, Verse, Span
- Custom
- generate Torah reading schedule
- tests based on Shulchan Aruch
- tests using calendar tables
- added CODEOWNERS

## [] - 2018-07-30
- calendar: finished "tying the knot" cleanup
- NonPeriodicNumberSystem, NonPeriodicPoint and NonPeriodicVector
- renamed AngleVector to Rotation, AngleNumberSystem.Point to Position, NumberSystem.Interval to Vector
- added some home-grown type-classes
- more property-based tests
- converted TODOs into issues
- cleanup

## [] - 2018-05-14
site:
- bring viewer back from a separate repository
- switched to Gradle
- switched to Git
- switched to ScalaTest
- added LICENSE.md
- Linearizer

- ## [] - 2018-04-05 - 04-12
- calendar: merged the paper repository back into the monorepo
- found a version of Saxon that can handle DocBook XSLT
- docbook: started on a Gradle DocBook plugin based on Gradle scripts floating around

## [0.1.1] - 2017-12-03
calendar:
- split calculations from roundings
- Moon calculations
- very rough first attempt at finding sign reversal point of a function
- using ScalaTest plugin (com.github.maiflai.scalatest)
- added some property-based tests
- publishing to Bintray

## [] - 2017-09-17
calendar:
- switched to Gradle
- switched to Git
- added LICENSE
- added Travis configuration
- used F-bounded type parameters, projections and self-types to encode family polymorphism for Calendar and NumberSystem
- used type parameters to encode Interval and Point in the NumberSystem family
- [yay!] split family members out of the family trait
- derived Calendar from TimeNumberSystem to tie the knot
- merged everything other than the paper into the same repository again
- introduced BigRational and used it for the NumberSystem's operations
- [yay!] Number is a sum of (a sequence of) signed terms which is not assumed to be in 'normal' form
- all operations in NumberSystem are done in terms of sequences
- Rosh Hashana corrections *really* externalized
- introduced Zodiac
- coded more astronomical calculations from Rambam
- more tests
- cleanup

NOTE: Scala compiler crashed (!!!) with the typing on Number/TimeNumber.
When family members are inside the family trait, it gets in the way (Family.this.T...);
To work around it, I tried using [T <: NumberSystem[T], N <: T#Number[T, N]].
It did the trick - I think - on the NumberSystem, but once I modified TimeNumberSystem in this way,
I got compiler crash!
Splitting the NumberSystem and TimeNumberSystem *helped* the knot-tying:
everything compiles and works!

## [] - 2016-05-08
- Rambam schedule generator

## [] - 2015-02-08
- split viewer into a separate repository
- split the paper and dates into separate repositories
- expressed head digit range constraints in types
- Rosh Hashana corrections externalized

## [] - 2015-01-29
- collector: initial check-in (again)
- XML support

## [] - 2014-06-23
- introduced ScalaTest
- adding tests directly from Rambam's text
- added scala-xml dependency
- Angle enhancements
- generalizing mixed-range numbers: Point, Interval, ScalarNumber
- NumberSystem, TimeNumberSystem, AngleNumberSystem
- Calendar ties the family polymorphism knot

## [] - 2014-02-04
- XML support improvements
- site: back to using (modern) Jersey
- Storage
- Paths
- caching

## [] - 2013-07-29
- more general-purpose XML utilities
- metadata-driven XML -> HTML transformation

## [] - 2013-02-27
- calendar: completed conversion to Scala
- code reorganization and cleanup
- no more webapp
- playing with family polymorphism
- more generated tables
- PDF and HTML generation tweaks

## [] - 2012-04-27
- texts: importers improvement
- XML parsing improvements
- structure description improvements
- selection

## [] - 2012-02-07
- migrating the paper

## [] - 2011-12-30
- site: Scalate
- migrating the paper (from where?)
- split out 'judaica' and 'typesetting' papers

## [] - 2011-11-24
- calendar: conversion to Scala
- Angle enhancements
- introduced Numbered[T <: Numbered[T]] and MomentT[..., T <: MomentT[...,T]]
- calendar: paper in DocBook format
- DocBook processing with Maven plugin
- generating tables and including them in the paper

## [] - 2011-06-15
- site: straight Servlets, no Jersey
- Names/Name
- XML parsing methods

## [] - 2011-02-23
- split parsing and adding of the metadata
- added XML files with Chumash structure (chapter, week, day)

## [] - 2011-01-20
- calendar: sun module

## [] - 2010-09-01
- site: Jersey webapp
- selectors
- text structure/metadata

## [] - 2010-08-06
- site: initial check-in
- Tanach importer using TagSoup with ScalaXml

## [] - 2010-04-04
- specialized JewishDate and GregorianDate
- calendar: modularization

## [] - 2009-10-25
- webapp

## [] - 2009-05-08
- molad
- Rosh Hashanah corrections
- Birkat HaChama
- Gregorian calendar and conversions
- seasons
- Google-importable iCal

## [] - 2009-04-02
- calendar: initial check-in
