#Jewish Calendar
(Understanding the ignored for fun and schar.)

##Goals

I want to be able to do various things with the fixed Jewish calendar:

-[x] code Rambam's calculations (and verify Rambam's numerical examples)
-[x] convert dates from Jewish calendar to Gregorian and back
-[x] list [dates of Birchas Hachamo](https://docs.google.com/document/d/1hpPZ0LYU3p8a-LJRXEqzXig-VQthkQ_MkaY79PMqr-Y/edit?hl=en_US)
* produce monthly calendars with both kinds of dates
* calculate schedule of learning Rambam
* calculate haftorah and torah reading schedule
* automate vestos calculations

For my needs, the best source of information on the Jewish Calendar turned out
to be Rambam, the Laws of the Sanctification of the Moon, Chapters 6-10.
The book "Calendrical Calculations" was also interesting, but algorithms and
formulas from it were not used directly.

I want to do certain things with the astronomical calendar as described by Rambam
in Chapters 11-19:

* code Rambam's calculations (and verify Rambam's numerical examples)
* analyze correspondence with the fixed calendar
* reconstruct models that Rambam uses, in contemporary (ancient Greek?) terminology and notation
* describe the models with diagrams and formulae
* reconstruct parameters of the models from the numbers given by Rambam 
* verify Rambam's tables and rounding decisions his models
* extract Rambam's criteria of visibility of the new moon
* see how Rambam's calculations of the moon visibility compare to the current models
* translate relevant chapters into English
* calculate halachic times for a day (zmanim)


##Early Jewish Years

Jewish years before the adoption of the fixed calendar were structured by the Bes Din.
I'd like to have an official record of what they were. Rabbi Hai Gaon? Seder Olam?

##Zmanim

There is an Android application; it references some kind of an "engine".
Mushinsky knows something too.

##Joda Time

As a good open-source citizen I probably should integrate my implementation of
the Jewish calendar into [Joda Time](http://joda-time.sourceforge.net/) or
[JSR 130](http://jcp.org/en/jsr/detail?id=310) (although the latter is inactive,
the article [New Java Date and Time API](http://today.java.net/pub/a/today/2008/09/18/jsr-310-new-java-date-time-api.html) notwithstanding). 

##Google Calendar Integration

It'd be nice to integrate with the Google Calendar.

* [iCalendar](http://en.wikipedia.org/wiki/ICalendar)
* [iCalendar RFC](http://tools.ietf.org/html/rfc2445)
* [Revised iCalendar RFC](http://tools.ietf.org/html/draft-ietf-calsify-rfc2445bis-08)
* [iCal4j](http://ical4j.sourceforge.net/introduction.html)
* [Publish web content events in iCalendar format](http://www.google.com/support/calendar/bin/answer.py?hl=en&answer=48526)
* [GData Java library](http://code.google.com/apis/gdata/client-java.html)
