##Goals

I want to be able to do various things with the fixed Jewish calendar:

* convert dates from Jewish calendar to Gregorian and back
* produce monthly calendars with both kinds of dates
* list [dates of Birchas Hachamo](https://docs.google.com/document/d/1hpPZ0LYU3p8a-LJRXEqzXig-VQthkQ_MkaY79PMqr-Y/edit?hl=en_US)
* calculate haftorah and tora reading schedule
* calculate schedule of learning Rambam
* automate vestos calculations

For my needs, the best source of information on the Jewish Calendar turned out
to be Rambam, the Laws of the Sanctification of the Moon, Chapters 6-10.
The book "Calendrical Calculations" was also interesting, but algorithms and
formulas from it were not used directly.


###Joda Time

As a good open-source citizen I probably should integrate my implementation of
the Jewish calendar into [Joda Time](http://joda-time.sourceforge.net/) or
[JSR 130](http://jcp.org/en/jsr/detail?id=310) (although the latter is inactive,
the article [New Java Date and Time API](http://today.java.net/pub/a/today/2008/09/18/jsr-310-new-java-date-time-api.html)
notwithstanding). 
