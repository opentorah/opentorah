---
layout: home
---

The Open Torah project produces a number of websites, papers and code packages.

## Websites ##

In addition to the main website www.opentorah.org, the project maintains:
- digital edition of Rabbi Wichnin's Chumash Questions book: www.chumashquestions.org;
- archive documents from the early history of Chabad
 (including the arrests of the Alter Rebbe in 1798 and 1800): www.alter-rebbe.org.

## Papers ##
- [paper](/paper/dream/dream.html) describing ideas (dating from 1991) for the dream environment for
   working with Jewish texts;
- companion [paper](/paper/typesetting/typesetting.html) on typesetting Jewish texts;
- [paper](/paper/calendar/calendar.html) on Rambam's Laws of the Sanctification of the Moon;
  goals of the paper:
  - reconstruct models that Rambam uses and their parameters from the Rambam's text;
  - identify misprints in the traditional editions;
  - see how Rambam's calculations of the moon visibility compare to the current models.

## Code ##
- [code](https://github.com/opentorah/opentorah/tree/master/collector) that generates and serves the www.alter-rebbe.org website;
- [code](https://github.com/opentorah/opentorah/tree/master/texts) for working with specific Jewish texts and for calculating Torah reading/learning schedules;
- [code](https://github.com/opentorah/opentorah/tree/master/core/src/main/scala/org/opentorah/calendar) implementing Rambam's calculations with dates, time intervals and angles and related algorithms.

## DocBook ##
- desire to publish texts in multiple formats was part of the motivation for writing
  [Gradle DocBook plugin](https://github.com/opentorah/opentorah/tree/master/docbook) in general
  and its support for mathematics in DocBook in particular;
  after the switch from DocBook to Asciidoc the plugin is no longer used or developed.
