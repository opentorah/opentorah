# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.7.10] - 2019-09-15
- MathJax ImagePreloader should refuse to handle non-DOMSource images;

## [0.7.9] - 2019-09-15
- switched from Travis to GitHub Actions;

## [0.7.8] - 2019-07-04
- MathML configuration for HTML;
- MathJax ex size;
- parsing errors logging;
- dependencies and up-to-date determination for the `processDocBook` task;

## [0.7.7] - 2019-06-26 - MathJax for PDF!
- wrap TeX and AsciiMath using an XML filter;
- Node support (inspired by the Gradle Node plugin);
- install `mathjax-node` Node module and dependencies;
- determine display mode from DocBook context (including for MathML);
- typeset math running Node in an external process;
- convert SVG sizes mathjax produces to those FOP understands;
- retrieve, extract and load platform-specific J2V8 library for native Node bindings;
- MathJax configuration DSL;

## [0.7.0] - 2019-03-10
- cleanup

## [0.6.9] - 2019-02-24
- multiple documents support;
- per-section defaults and default customization files;
- intercept Saxon warnings and log them as info;
- removed local copy of the JEuclid FOP plugin;
- substitutions for PDF metadata.

## [0.6.8] - 2019-02-21
- renamed `documentName` to `document`, `cssFileName` to `cssFile`;
- `use.id.as.filename` by default;
- added hyphenation rules dependency;
- suppressed "font directory" warning (set baseUri);
- chunk quietly and fast;
- lifecycle and warning logging with logging ErrorListener;
- skip "main" output file when chunking in XSLT 1.0 - and supply a writer in XSLT 2.0;
- cleanup.

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

## [0.4.1] - 2019-02-11
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
- started working on DocBook Gradle Plugin
