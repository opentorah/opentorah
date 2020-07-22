# Changelog

Modules covered: `calendar`, `calendar-service`, `schedule` and `calendar-paper`.

## [Unreleased]

## [0.3.3] - 2020-05-31
- simplified directory structure
- moved common code to appropriate modules
- moved Tanach and Rambam structures and support into the `texts` module
- moved schedule-related stuff into the `schedule` module
- cleaned up SpecialDay (`schedule`)
- cleaned up and updated some dependencies
- removed ScalaTags (`calendar-service`)

## [0.3.1] - 2020-04-26
- packages renamed to org.opentorah
- split out jewish-calendar.org website
- merged into the `opentorah` monorepo
- converted service to ZIO

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
- merged jewish-calendar.org site into this repository
- configuring Jekyll (and MathJax) for the project's site
- more documentation 
- moved DocBook plugin into a separate repository
- added to schedule: Sefer Hamitzvos, Molad, Shabbos Mevarchim, seasons, year type and cycles
- normal forms of numbers simplified and normalization centralized
- exactification
- cleanup

## [0.2.5] - 2018-12-31
- structure of the Rambam's Mishneh Torah and mapping of the lessons to Sefer Hamitzvos
- Rambam learning schedules
- Psalms schedule
- CHANGELOG.md

## [0.2.0] - 2018-12-16
- simplified POM configuration and Bintray publishing
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
- finished "tying the knot" cleanup
- NonPeriodicNumberSystem, NonPeriodicPoint and NonPeriodicVector
- renamed AngleVector to Rotation, AngleNumberSystem.Point to Position, NumberSystem.Interval to Vector
- added some home-grown type-classes
- more property-based tests
- converted TODOs into issues
- cleanup

## [] - 2018-04-12
- merged the paper repository back into the monorepo
- found a version of Saxon that can handle DocBook XSLT

## [0.1.1] - 2017-12-03
- split calculations from roundings 
- Moon calculations
- very rough first attempt at finding sign reversal point of a function
- using ScalaTest plugin (com.github.maiflai.scalatest)
- added some property-based tests
- publishing to Bintray

## [] - 2017-09-17
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
- split the paper and dates into separate repositories
- expressed head digit range constraints in types
- Rosh Hashana corrections externalized

## [] - 2014-06-23
- introduced ScalaTest
- adding tests directly from Rambam's text
- added scala-xml dependency
- Angle enhancements
- generalizing mixed-range numbers: Point, Interval, ScalarNumber
- NumberSystem, TimeNumberSystem, AngleNumberSystem
- Calendar ties the family polymorphism knot

## [] - 2013-02-27
- completed conversion to Scala
- code reorganization and cleanup
- no more webapp
- playing with family polymorphism
- more generated tables
- PDF and HTML generation tweaks

## [] - 2011-11-24
- conversion to Scala
- Angle enhancements
- introduced Numbered[T <: Numbered[T]] and MomentT[..., T <: MomentT[...,T]]
- paper in DocBook format
- DocBook processing with Maven plugin
- generating tables and including them in the paper

## [] - 2011-01-20
- sun module

## [] - 2010-04-04
- specialized JewishDate and GregorianDate
- modularization

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
- Initial check-in
