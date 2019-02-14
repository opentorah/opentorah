## DocBook XSLT Issues ##

_Footnotes_ DocBook stylesheets render each footnote on a separate line; when there are many of
them, this doesn't look good. Is there a way to render footnotes as a continuous block?
Which XSLT template renders footnotes to XSL-FO and how do I twist it to not break the line?

_Honorifics_ DocBook stulesheets render honorifics in bibliography with dots after them,
which doesn't look good for 'Rabbi' :) It can be removed
(as in [chumashquestions.org](http://www.chumashquestions.org/)), but has then to be typed in
for all other honorifics. How do I augment the XSLT template to skip the dot after listed
honorifics (Rabbi, Rebitzin, Reb)?
 
_Edition_ In bibliography, when I give "edition" as "3rd", it is printed as "3rd", not "3rd edition".
Am I supposed to spell it out everywhere?!

_Links_ Links (e.g., bibliorefs) in PDF output are clickable, but it is not obvious.
Can I persuade DocBook stylesheets to make them underlined with blue or something?

_Revision_ Add date of revision to the title page.

_Tables_
- force monospaced font
- force good font for the degree values (which tend to go Times or something)
- days column needs to be aligned right
- set column width?
- use more HTML-like tables?
