---
layout: home
title: Understanding the ignored for fun and schar
---

## The paper ##

Paper on Rambam's Laws of the Sanctification of the Moon:
- [HTML](/paper/html/)
- [PDF](/paper/pdf/calendar.pdf)
- [EPUB2](/paper/epub2/calendar.epub)

Goals of the paper:
- reconstruct models that Rambam uses and their parameters from the Rambam's text;
- identify misprints in the traditional editions; 
- see how Rambam's calculations of the moon visibility compare to the current models.

Desire to publishing this paper in multiple formats was part of the motivation for writing
[Gradle DocBook plugin](https://github.com/dubinsky/podval-docbook-gradle) in general and its support
for mathematics in DocBook in particular. It also influenced my thinking on
[publishing papers on the web](http://dub.podval.org/2019/05/06/publishing-papers-on-web-2.html).

## The Code ##

![](https://github.com/opentorah/calendar/workflows/CI/badge.svg)

Code that makes possible calculations with dates, time intervals and angles exactly as they are
described by Rambam is available:
- [Repository](https://github.com/opentorah/calendar)
- [Changelog](/CHANGELOG.md)
- [ScalaDoc](/scaladoc/org/podval/calendar/index.html)
- [Examples](/examples/)
- [Bintray](https://bintray.com/beta/#/dubinsky/org.opentorah/calendar?tab=overview)
- [JCenter](https://jcenter.bintray.com/org/opentorah/calendar/)

The code is written in Scala and uses extendable family polymorphism encoding with family members in
separate files inspired by a Stackoverflow
[post](https://stackoverflow.com/questions/1154571/scala-abstract-types-vs-generics/10891994#10891994)
by [ayvango](https://stackoverflow.com/users/837133/ayvango). It:
- implements Rambam's calculations for arithmetic calendar (chapters 6-10);
- implements Rambam's calculations for astronomical calendar (chapters 11-19);
- has Rambam's numerical examples as unit tests;
- provides conversions between Jewish and secular (Gregorian) dates;
- supports [sanctification of the Sun](http://dub.podval.org/2019/07/18/sanctification-of-the-sun.html)
 calculations;
- generates Torah and Haftorah reading schedule for a number of customs;
- generates schedule of learning Rambam;

Enhancements being considered are:
- calculation of halachic times (zmanim)
  - raw data for the hazot calculations is obtainable from the [Naval Oceanography Portal](http://www.usno.navy.mil/USNO/astronomical-applications/data-services/rs-one-year-us);
    it is produced in accordance with the algorithm described in the Meese's book, I think...
  - there is [another source of data](http://www.timeanddate.com/worldclock/sunrise.html)
- integration with [Joda Time](http://joda-time.sourceforge.net/) or
 [JSR 130](http://jcp.org/en/jsr/detail?id=310) (although the latter is inactive, the article
      [New Java Date and Time API](http://today.java.net/pub/a/today/2008/09/18/jsr-310-new-java-date-time-api.html)
      notwithstanding).
- integration with Google Calendar:
  - [iCalendar](http://en.wikipedia.org/wiki/ICalendar)
  - [iCalendar RFC](http://tools.ietf.org/html/rfc2445)
  - [Revised iCalendar RFC](http://tools.ietf.org/html/draft-ietf-calsify-rfc2445bis-08)
  - [iCal4j](http://ical4j.sourceforge.net/introduction.html)
  - [Publish web content events in iCalendar format](http://www.google.com/support/calendar/bin/answer.py?hl=en&amp;answer=48526)
  - [GData Java library](http://code.google.com/apis/gdata/client-java.html)


## Schedule ##

There are some schedule-related issues to look into:
- Magen Avraham 428:4 (6): When Pesach falls on Shabbat, so that residents of the Land of Israel
  read Shmini on the Shabbat following it, there are those that split Tazria and Metzora, and
  there are those that split Behar and Bechukotai, and if the year is leap, they split Matot and Masai;
- Baladi (Yemenite): instead of Matot and Masai combine Chukat and Balak;
- Daradaim (following Rabbi Saadia Gaon): instead of combining Matot and Masai, add to Korach Chukat
 to 20:21, and next Shabbos read the rest of Chukkat and Balak;
- 3 small aliyot (for 2/5/Shabbos day): look into and implement differences in customs;
- 3-year cycle: is anybody orthodox completing Torah cycle in three years? If yes, maybe we should generate that
 schedule too. There is a [list of Haftarot](https://faculty.biu.ac.il/~ofery/papers/haftarot3.pdf);

[Sources](/sources.md) for the schedule algorithms.
