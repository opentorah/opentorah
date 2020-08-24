# Changelog

Modules covered: `store`, `metadata`, `xml`, `tei`, `util`, `texts`, `dream-paper` and `typesetting-paper`.

## [Unreleased]

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

