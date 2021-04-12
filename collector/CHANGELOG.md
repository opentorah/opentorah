# Changelog

## [Unreleased]

## [0.2.6] - 2021-0

- use the 'site' module;
- code highlighting using `highlightjs`;
- PrettyPrinter encodes XML specials in attributes, character data and preformatted blocks;
- `DoubleEscapeHtmlNodeExtension` removed;

## [0.2.5] - 2021-02-28
- site is now dynamic!
- double-escape XML tag start characters and such in Markdown code blocks;
- PrettyPrinter supports preformatted elements;
- and doesn't let Paiges to mis-indent them;
- ZIOfication;

## [0.2.3] - 2021-02-02
- serve using local Docker
- facsimiles - hierarchically 

## [0.2.1] - 2021-01-31
- use correct thread-pools to avoid ZIO dead-locks with nested `unsafeRun()` calls in CPU-constrained situation;
- use latest Cloud Run Gradle plugin to run in the local Docker;

## [0.2.0] - 2021-01-27
My attempt at re-using Store and friends for texts other than the alter-rebbe archive failed:
it was not general enough, and too inflexible to be used even in its original context.
Code in the 'collector' module was re-written, and 'store' module emptied out.
- Service class enhanced to serve dynamic content;
- Main and Cutter moved into the alter-rebbe.org repository;
- site XML files there adjusted to the new format;

## [0.1.62] - 2020-12-20
- hybrid store/collector new generation;
- selectors in metadata, not in the store file;

## [0.7.60] - 2020-12-13
- synchronizing local directory to a Google Storage bucket;
- use CRS32c to detect changes in uploaded files;
- introduced `.gsignore` file to exclude files from the Google Store sync;
- consolidated build functionality (verify/generate/upload/serve site) in the Main object;
- writing and reading References;
- cleanup;

## [0.7.56] - 2020-12-06
- converting TEI to HTML in code (no more CETEIcean);
- generating HTML in code (no more Jekyll for the TEI files);
- generating HTML from Markdown files (no more Jekyll for them);
- generating CSS from SCSS at build time (no more Jekyll!);
- folded `collector-service` into `collector`;  
- serving local files without Jekyll (which is no longer used for anything);
- UI improvements;
- keep one instance of the service always warm (no more Cloud Scheduler);
- using CloudRun Gradle plugin;

## [0.1.53] - 2020-08-23 (`collector-service`)
- tracking StaticFile fix;

## [0.1.52] - 2020-07-03 (`collector-service`)
- instructions for the `collector-service` setup;
- logging using Logback to integrate with Cloud Run logging;
- use structured logging (with request trace);
- log environment variables retrieval;
- log files retrieved from the documents site (with timing);
- keep the service warm using Cloud Scheduler;

## [] - 2020-04-26
- moved into the `opentorah` monorepo
- created `collector` and `collector-service` modules

## [] - 2020-04-21
- pretty-print Stores read from files back into them
- site hierarchy
- introduce Site class
- write site in pull mode
- process pb elements
- reports in TEI
- collector service

## [] - 2020-03-09
- source TEI documents processed into TEI documents for display
- moved packages to org.opentorah
- allow and report ref-less names
- report misnamed nameds
 
## [] - 2019-12-08
- facsimiles hosted externally
- facsimile checks disabled
- 'role' attribute on Named and lists
 
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

## [] - 2019-04-23
- CETEIcean behaviours for pb etc.
- indexes in TEI
- navigation links

## [] - 2019-03-25
- switched to Gradle
- switched to Git
- TEI 'ref' attribute converted to hyperlinks
- transcriber in the TEI header
- switched to CETEIcean

## [] - 2015-01-29
- initial check-in (again)
- XML support
