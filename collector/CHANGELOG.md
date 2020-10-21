# Changelog

Modules covered: `collector` and `collector-service`.

## [Unreleased]

## [0.1.55]
- converting TEI to HTML in code (no more CETEIcean);
- generating HTML in code (no more Jekyll for the TEI files)
- UI improvements;

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
