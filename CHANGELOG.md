# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
- switched from http4s to zhttp;
- switched to Scala 3 and its syntax!
- ScalaTest 2.2.10 uses current FlexMark.

## [0.2.6] - 2021-08-17
- consolidated 'util', 'xml' and 'metadata' modules into 'base';
- 'store' is in the 'base';
- consolidated 'calendar-paper' into 'calendar';
- consolidated 'fop' into 'docbook';
- consolidated 'calendar-service' into 'schedule';  
- consolidate 'dream' and 'typesetting' papers into 'texts';
- added 'site' module with Site/HtmlTheme/HtmlContent/Viewer and Markdown support;
- added 'docs' module;
- added theme files to the project's site;
- added SASS -> CSS compilation to the project's site;
- collector and DocBook plugin use the 'site' module;
- support for MathJax3;
- code highlighting using `highlightjs`;
- working on direct DocBook -> PDF transformation (without plugin);
- correct `jib.from.image`;
- centralized dependency versions using Gradle 7 features;
- dependency updates;
- code adjusted to compile with Scala 3 (using 
  [patched Gradle](https://github.com/gradle/gradle/pull/18001) and Scala 3.0.3 nightlies);

## [0.2.4] - 2021-02-04
- moved from JCenter/Bintray to Maven Central (see http://dub.podval.org/2021/02/04/bintray-is-dead.html)!
- no tests in master CI
- no artifact uploads from GitHub Actions

## [0.2.1] - 2021-01-28
- 'collector' re-written

## [0.1.66] - 2021-01-14
- Scala 2.13+
- split-file family polymorphism encoding in 'calendar' removed :(

## [0.1.62] - 2020-12-20
- calendar not using store;

## [0.1.56] - 2020-12-06
- using CloudRun Gradle plugin;

## [0.1.53] - 2020-08-23
- switched to Logback logging;
- added blog and moved posts from `alter-rebbe.org` here;
- added Bintray upload to the `CI` GitHub action
- added JIB image upload and Cloud Run Deploy to the `CI` GitHub action 

## [] 2020-04-28
- centralized module setup (scala, library, service, mdoc, paper Gradle scripts)
