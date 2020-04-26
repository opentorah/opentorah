# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.3.1] - 2020-04-26
- split out jewish-calendar.org website

## [0.3.0] - 2019-02-13
- Psalms for the day of the week, year's molad and delay
- website index.html consolidation
- use sync for publishing the paper into docs
- using DocBook Plugin!
- using JEuclid: MathML in PDF!
- font that has all required scripts
- split build.gradle (and removed POM)

## [0.2.9] - 2019-01-23
- using Cats and Algebra for testing the laws
- optimization: Years are cached; Month/Day get their Year/Month at construction if it is known
- moved jewish-calendar.org into the docs directory in *this* repository
- configuring Jekyll (and MathJax) for the project's site
- more documentation 
- moved docbook plugin into a separate repository
- added to schedule: Sefer Hamitzvos, Molad, Shabbos Mevarchim, seasons, year type and cycles
- normal forms of numbers simplified and normalization centralized
- exactification
- cleanup

## [0.2.5] - 2018-12-31
- metadata for Rambam's Mishneh Torah
- parse Mishneh Torah metadata
- metadata mapping from lessons in Mishneh Torah to Sefer Hamitzvos
- parse the mapping
- calculate Rambam learning schedules
- parse Psalms metadata
- Psalms schedule
- CHANGELOG.md

## [0.2.0] - 2018-12-16
- updated dependencies
- POM improvements 
- simplified Bintray publishing
- built and uploaded custom Google Cloud Build Gradle builder that supports Gradle 5.0
- cleaned up cloudbuild.yaml
- code reorganization and cleanup
- moved TODOs into issues
- credits and sources
- "caches"
- annotate readings with their sources (Shabbos, special day, etc.)
- chapters for the Writings books
- full metadata for the Book of Psalms

## [] - 2018-12-06
- using http4s
- using Scalatags
- CalendarService
- packaging service as Docker image (with configurable port)
- configure Google Cloud Build: cloudbuild.yaml
- integrate with Google Cloud Build on pull request merge
- restart VM instance from the Cloud Build
- better Custom minimization
- Chumash Chitas
- multi-lingual UI; LTR/RTL 

## [] - 2018-11-14
- added metadata-related stuff from the "Digital Judaica" project
- Language, LanguageSpec and Custom 
- parsing metadata using Holders
- binding metadata 
- Tanach metadata
- metadata for regular Haftarot
- special days of the Jewish calendar and their effect on the Torah reading
- generate Torah reading schedule
- translated of the relevant paragraph from Shulchan Aruch;
- tests based on Shulchan Aruch
- tests using calendar tables
- code reorganization
- CODEOWNERS 

## [] - 2018-07-30
- finished "tying the knot" cleanup
- NonPeriodicNumberSystem, NonPeriodicPoint and NonPeriodicVector
- renamed AngleVector to Rotation
- renamed AngleNumberSystem.Point to Position
- renamed NumberSystem.Interval to Vector
- added some home-grown typeclasses
- converted TODOs into issues
- cleanup
- more property-based tests

## [] - 2018-04-13
- switched from Hg to GIT
- merged the paper repository back into the monorepo
- started on a Gradle DocBook plugin based on Gradle scripts floating around 
- found a version of Saxon that can handle DocBook XSLT

## [0.1.1] - 2017-12-03
- split calculations from roundings 
- Moon calculations
- very rough first attempt at finding sign reversal point of a function
- position names
- added some property-based tests
- publishing to Bintray

## [] - 2017-09-17
- switched to Gradle
- added LICENSE
- added Travis configuration
- used F-bounded type parameters and projections to encode family polymorphism for Calendar and NumberSystem 
- used type parameters to encode Interval and Point in the NumberSystem family
- split family members out of the familty trait
- derived Calendar from TimeNumberSystem to tie the knot
- merged everything other than the paper into the same repository again
- introduced BigRational and used it for the NumberSystem's operations
- Number is a sum of (a sequence of) signed terms which is not assummed to be in 'normal' form
- all operations in NumberSystem are done in terms of sequences in 
- more tests.
- cleanup

NOTE: Scala compiler crashed (!!!) with the typing on Number/TimeNumber.
When family members are inside the family trait, it gets in the way (Family.this.T...);
To work around it, I tried using [T <: NumberSystem[T], N <: T#Number[T, N]].
It did the trick - I think - on the NumberSystem,
but once I modified TimeNumberSystem in this way - I got compiler crash!
Splitting the NumberSystem and TimeNumberSystem *helped* the knot-tying:
everything compiles and works!

## [] - 2016-05-08
- Rambam schedule generator

## [] - 2015-02-08
- split the paper and dates into separate repositories
- expressed head digit range constraints in types
- no more Servlets

## [] - 2014-06-23
- introduced ScalaTest
- code cleanup
- updated dependencies
- adding tests directly from Rambam's text
- added scala-xml dependency
- generalizing mixed-range numbers: NumberSystem, Point, Interval, ScalarNumber
- TimeNumberSystem
- AngleNumberSystem
- Calendar ties the family polymorphism knot

## [] - 2013-02-27
- completed conversion to Scala
- code reorganization and cleanup
- code reorganization
- PDF and HTML generation tweaks
- playing with family polymorphism.

## [] - 2011-11-24
- converted to Scala
- paper in DocBook format
- DocBook processing with Maven plugin
- generating tables and including them in the paper
- Angle enhancements

## [] - 2010-04-04
- modularization

## [] - 2009-10-25
- Rosh Hashanah delays
- molad
- seasons
- Birkat HaChama
- Gregorian calendar
- Google-importable iCal
- checkstyle and findbugs
- tests

## [] - 2009-04-02
- Initial check-in
