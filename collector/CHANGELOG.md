# Changelog

## [Unreleased]

## [0.7.60] - 2020-12-13
- synchronizing local directory to a Google Storage bucket;
- use CRS32c to detect changes in uploaded files;
- introduced `.gsignore` file to exclude files from the Google Store sync;
- consolidated build functionality (verify/generate/upload/serve site) in the Main object;
- working on writing and reading References;
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
