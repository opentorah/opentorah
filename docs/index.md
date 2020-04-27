---
layout: home
title: Digital Judaica Done Right
---

There are a few ongoing projects:
- digital edition of Rabbi Wichnin's [Chumash Questions](https://www.chumashquestions.org/) book;
- [archive documents](https://www.alter-rebbe.org) of the arrests of the Alter Rebbe in 1798 and 1800;   

Ideas (dating from 1991) for the dream environment for working with Jewish texts are the
subject of two papers: general ([HTML](/judaica/html/index.html) [PDF](/judaica/pdf/judaica.pdf)
[EPUB](/judaica/epub/judaica.epub)) and typesetting-related ([HTML](/typesetting/html/index.html)
[PDF](/typesetting/pdf/judaica.pdf) [EPUB](/typesetting/epub/judaica.epub)).
Code implementing some of those ideas is in a [GitHub repository](https://github.com/opentorah/store).

[SBL font](http://www.sbl-site.org/educational/BiblicalFonts_SBLHebrew.aspx) is
 needed for viewing Tanach.


## Calendar Paper ##

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

Code that makes possible calculations with dates, time intervals and angles exactly as they are
described by Rambam is available: [Repository](https://github.com/opentorah/calendar).
