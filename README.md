# Jewish Calendar
(Understanding the ignored for fun and schar.)

## Goals

I want to be able to do various things with the fixed Jewish calendar
as described by Rambam in the Laws of the Sanctification of the Moon, Chapters 6-10:

- [x] code Rambam's calculations (and verify Rambam's numerical examples)
- [x] convert dates from Jewish calendar to Gregorian and back
- [x] list [dates of Birchas Hachamo](https://docs.google.com/document/d/1hpPZ0LYU3p8a-LJRXEqzXig-VQthkQ_MkaY79PMqr-Y/edit?hl=en_US)
- [x] generate Torah and Haftorah reading schedule
- [ ] generate schedule of learning Rambam
- [ ] generate monthly calendars with both kinds of dates

I want to do certain things with the astronomical Jewsih calendar
as described by Rambam in the Laws of the Sanctification of the Moon, in Chapters 11-19:

- [ ] code Rambam's calculations (and verify Rambam's numerical examples)
- [ ] verify Rambam's tables and rounding decisions against his models
- [ ] analyze impact of various misprints on the sighting calculations 
- [ ] analyze correspondence with the fixed calendar
- [ ] reconstruct models that Rambam uses, in contemporary (ancient Greek?) terminology and notation
- [ ] describe the models with diagrams and formulae
- [ ] reconstruct actual parameters of the models from the numbers given by Rambam 
- [ ] see how Rambam's calculations of the moon visibility compare to the current models
- [ ] translate relevant chapters into English
- [ ] calculate halachic times (zmanim)

## Credits

I want to acknowledge people who contributed to the content of this work - and to the fact of its existence ;)
If your name should be on this list but isn't, please forgive me: the omission is not intentional.

- Mordechai Goldin - for providing office space and computer equipment when I, as a Yeshiva student, had neither;
- Ilia Pomansky - for encouraging this work at the early stages (in 1991);
- my wife Nina - for listening to my wild ideas, for her patience, and for help with math;
- my daughter Menucha Rochel - for assistance with the translation;
- Rabbi Michael Koretz - for stimulating discussions, encouragement, and extensive research
into customs of Torah/Haftarah reading;
- Dr. Michael Partensky - for encouragement and advise;
- Dr. Peter Ofman - for asking questions that prodded me to revive this project in 2011, after years of hiatus;
- Aron Matskin - for a discussion during his visit on Rosh Chodesh Mar Cheshvan 5772;
- Rabbi Chaim Prus - for causing me to investigate which number does the year of Creation has - 0 or 1.


## Sources

_Torah reading_: Shulchan Aruch Orach Chaim (and Mishna Brura):
425:1-2 (4, 8, 12), 428:4,6,8, 488:3; 490:1, 494:1-2; 559:4,
584:2 (8), 601:1, 621:1 (4), 622:2, 659 (4), 662:3 (4),
663:1,3 (4), 668:2, 669
Chanukah: 684 (3, 8, 14, 17),
693:4          

TODO Rambam corresponding to Shulchan Aruch Orach Chayim 428
TODO https://he.wikipedia.org/wiki/הלוח_העברי
TODO https://en.wikibooks.org/wiki/Mathematics_of_the_Jewish_Calendar/The_Annual_Cycle_of_Torah_Readings
TODO https://orot.ac.il/sites/default/files/morashtenu/16-3.pdf

Book "Calendrical Calculations" was also interesting, but algorithms and
formulas from it were not used directly.

## Early Jewish Years

Jewish years before the adoption of the fixed calendar were structured by the Bes Din.
I'd like to have an official record of what they were. Rabbi Hai Gaon? Seder Olam?

## Zmanim

There is an Android application; it references some kind of an "engine".
Mushinsky knows something too.

Raw data for the hazot calculations is obtainable from the Naval Oceanography Portal at
 http://www.usno.navy.mil/USNO/astronomical-applications/data-services/rs-one-year-us
There it is produced in accordance with the algorithm described in the Meese's book, I think...
Another source of data is http://www.timeanddate.com/worldclock/sunrise.html.


## Joda Time

As a good open-source citizen I probably should integrate my implementation of
the Jewish calendar into [Joda Time](http://joda-time.sourceforge.net/) or
[JSR 130](http://jcp.org/en/jsr/detail?id=310) (although the latter is inactive,
the article [New Java Date and Time API](http://today.java.net/pub/a/today/2008/09/18/jsr-310-new-java-date-time-api.html) notwithstanding). 

## Google Calendar Integration

It'd be nice to integrate with the Google Calendar.

* [iCalendar](http://en.wikipedia.org/wiki/ICalendar)
* [iCalendar RFC](http://tools.ietf.org/html/rfc2445)
* [Revised iCalendar RFC](http://tools.ietf.org/html/draft-ietf-calsify-rfc2445bis-08)
* [iCal4j](http://ical4j.sourceforge.net/introduction.html)
* [Publish web content events in iCalendar format](http://www.google.com/support/calendar/bin/answer.py?hl=en&answer=48526)
* [GData Java library](http://code.google.com/apis/gdata/client-java.html)

## Publishing on the Web

See <http://blog.dub.podval.org/2011/11/publishing-papers-on-web.html> about technical details of the publication.
