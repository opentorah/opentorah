# Changelog

## [Unreleased]

## [0.7.56] - 2021-0
- `mathJax.isEnabled` renamed `mathJax.enabled`;
- `isJEuclidEnabled` renamed `jEuclidEnabled`;
- support for MathJax3;
- merged FOP plugin back into this module: nobody used it ;)
- use the 'site' module;
- `DoubleEscapeHtmlNodeExtension` removed;

## [0.7.53] - 2020-08-23
- use XML literals instead of strings
- pretty-printing DOM

## [0.7.52] - 2020-07-14
- parameters cleanup
- using Xerces for loading Sla XML

## [0.7.51] - 2020-06-26
- added output format `variants` (like `pdf-a4`)
- Layout cleanup

## [0.7.50] - 2020-05-31
- caching frameworks for DocBook processing
- cleaned up data generation
- cleaned up generated catalog etc.
- updated FOP to 2.5
- updated to Saxon 10 and used it for FOP 

## [0.7.49] - 2020-05-08
- merged into the `opentorah` monorepo
- moved packages to `org.opentorah`
- moved common code into appropriate modules
 
## [0.7.14] - 2019-12-02
- split FOP plugin into a separate module

## [0.7.13] - 2019-11-26
- force use of Xerces by Saxon always, including SVG   

## [0.7.12] - 2019-11-26
- force use of Xerces by Saxon   

## [0.7.11] - 2019-11-25
- update Gradle to 6.0.1; call metadataSources.artifact() explicitly to handle NodeJS
  repository (was done automatically in Gradle <6)
- update FOP to 2.4; exclude new JAI dependencies that rely on JBoss repository
  add equivalent dependencies from JCenter

## [0.7.10] - 2019-09-15
- MathJax ImagePreloader should refuse to handle non-DOMSource images

## [0.7.9] - 2019-09-15
- switched from Travis to GitHub Actions

## [0.7.8] - 2019-07-04
- MathML configuration for HTML
- MathJax ex size
- parsing errors logging
- dependencies and up-to-date determination for the `processDocBook` task

## [0.7.7] - 2019-06-26 - MathJax for PDF!
- wrap TeX and AsciiMath using an XML filter
- Node support (inspired by the Gradle Node plugin)
- install `mathjax-node` Node module and dependencies
- determine display mode from DocBook context (including for MathML)
- introduced Namespace
- typeset math running Node in an external process
- convert SVG sizes mathjax produces to those FOP understands
- retrieve, extract and load platform-specific J2V8 library for native Node bindings
- MathJax configuration DSL

## [0.7.0] - 2019-03-10
- cleanup

## [0.6.9] - 2019-02-24
- multiple documents support
- per-section defaults and default customization files
- intercept Saxon warnings and log them as info
- removed local copy of the JEuclid FOP plugin
- substitutions for PDF metadata

## [0.6.8] - 2019-02-21
- renamed `documentName` to `document`, `cssFileName` to `cssFile`
- `use.id.as.filename` by default
- added hyphenation rules dependency
- suppressed "font directory" warning (set baseUri)
- chunk quietly and fast
- lifecycle and warning logging with logging ErrorListener
- skip "main" output file when chunking in XSLT 1.0 - and supply a writer in XSLT 2.0
- cleanup

## [0.6.7] - 2019-02-20
- Layout/DocBook2 cleanup
- namespace-aware DocBook XSLT 1.0 stylesheets

## [0.6.6] - 2019-02-20
- merged XSL customizations, temporary directory, output directory and
  output formats configuration for XSLT 1.0 and 2.0 stylesheets
- versions of the XSLT stylesheets are now configurable

## [0.6.5] - 2019-02-19
- XSLT files and XML catalog customizable and available to Oxygen
- per-format parameters
- HTML with XSLT 2.0 stylesheets

## [0.6.0] - 2019-02-17
- resolving is delegated via generated XML catalog (and substitutions DTD) that can be used in Oxygen

## [0.5.0] - 2019-02-13
- cleanup and tests  
- using caching resolver from org.xmlresolver
- MathML in PDF via JEuclid FOP plugin

## [0.2.2] - 2019-02-10
- XSLT 2.0 DocBook stylesheets with Saxon 9 HE

## [0.2.1] - 2019-02-06
- EPUB3
- EPUB font embedding

## [0.2.0] - 2019-02-05
- configurable output formats
- better substitutions
- cleanup
- README.md
- publishing to plugin.gradle.org

## [] - 2019-01-25 - 01-31
- integration with Gradle Task Input/Output
- substitution of entities
- evaluation of processing instructions

## [] - 2019-01-09
- moved it into a separate repository

## [] - 2018-04-05 - 04-12
- started on a Gradle DocBook plugin based on Gradle scripts floating around 

