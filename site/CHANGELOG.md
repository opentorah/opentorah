# Changelog

Modules covered: `base`, `site`, `tei` and `texts`.

## [Unreleased]
- XML processing cleanup;

## [0.2.6] - 2021-08-17
- added 'site' module with Site/HtmlTheme/HtmlContent/Viewer and Markdown support;
- added theme files to the project's site;
- added SASS -> CSS compilation to the project's site;
- collector and DocBook plugin use the 'site' module;
- HtmlTheme supports MathJax;
- support to MathJax3;
- code highlighting using `highlightjs`;
- PrettyPrinter encodes XML specials in attributes, character data and preformatted blocks;
- `DoubleEscapeHtmlNodeExtension` removed;
- moved Store, By, Selector, Directory, WithSource and some Site/HtmlContent methods into the
  'store' package of the 'base' module;
- pretty-printing in Site;
- consolidated Site commands (including siteUpload) into SiteService;
- transforming DocBook -> HTML in DocBook.scala;
- working on the DocBook stylesheet and element transform;
- experimenting with the PDF generation with Playwright;
- allow allowing `RawXml` elements to have attributes;

## [0.2.0] - 2021-01-27
My attempt at re-using Store and friends for texts other than the alter-rebbe archive failed:
it was not general enough, and too inflexible to be used even in its original context.
Code in the 'collector' module was re-written, and 'store' module emptied out.

## [0.1.67] - 2021-01-15
- Attribute.Parsable and ensuing cleanup;
- everything is a Parsable now ;)

## [0.1.62] - 2020-12-20
- selectors in metadata, not in the store file;
- xml.Element extends both FromXml and ToXml;
- cleanup;

## [0.1.61] - 2020-12-14
- ToXml and Parsable cleanup;
- replaced Parsable.withInclude() with unlimited redirects via Element.canRedirect;
- From.url(..., isRedirect) to mark redirects in Context;
- Context.fromUrls; Store parsers do not have to keep track of the URLs (but still do);

## [0.1.60] - 2020-12-13
- made PrettyPrinter.chunkify() tail-recursive;

## [0.1.56] - 2020-12-07
- converting TEI to HTML;
- XML Namespace and Dialect cleanup;
- XML Attribute and Antiparser cleanup;

## [0.1.53] - 2020-08-23
- texts metadata parsers ZIOfied a little
- PrettyPrinter chunking cleanup
- gathered standard XML namespaces in the Namespace object
- abstracted pretty-printer over XML representation
- moved TEI support into the `tei` module
- introduced `Antiparser` AND USED IT THORUGHOUT

## [0.1.52] - 2020-07-14
- added PrettyPrinter.alwaysStackElements
- merged Attribute and AttributeX
- Attribute defaults and conversions cleanup
- XML parsing cleanup

## [0.1.51] - 2020-06-26
- fixed ZIO deadlock in the Rambam schedules
- deleted `importers` code but kept the notes: `importers.txt`
- XML processing cleanup

## [0.1.50] - 2020-06-01
- added README.md
- clean up SpecialDay
- clean up Haftarah

## [0.1.49] - 2020-05-08
- split out `metadata` module
- split out `texts` module

## [] - 2020-04-26
- moved www.opentorah.org into a separate repository
- merged everything into the `opentorah` monorepo
- 'xml' module

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

## [0.1.22] - 2020-03-08
- XML pretty-printing using Paiges
- TEI support enhanced
- parsing moved for Cats to ZIO
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

## [] - 2018-05-14
- bring viewer back from a separate repository
- switched to Gradle
- switched to Git
- switched to ScalaTest
- added LICENSE.md
- Linearizer

## [] - 2015-02-08
- split viewer into a separate repository

## [] - 2014-02-04
- XML support improvements
- back to using (modern) Jersey
- Storage
- Paths
- caching

## [] - 2013-07-29
- more general-purpose XML utilities
- metadata-driven XML -> HTML transformation

## [] - 2012-04-27
- importers improvement
- XML parsing improvements
- structure description improvements
- selection

## [] - 2012-02-07
- migrating the paper

## [] - 2011-12-30
- Scalate
- migrating the paper (from where?)
- split out 'judaica' and 'typesetting' papers

## [] - 2011-06-15
- straight Servlets, no Jersey
- Names/Name
- XML parsing methods

## [] - 2011-02-23
- split parsing and adding of the metadata
- added XML files with Chumash structure (chapter, week, day)

## [] - 2010-09-01
- Jersey webapp
- selectors
- text structure/metadata

## [] - 2010-08-06
- initial check-in
- Tanach importer using TagSoup with ScalaXml

