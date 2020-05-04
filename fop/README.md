# Mathematics plugin for FOP #

This was developed as a part of the DocBook plugin for Gradle;
see https://github.com/opentorah/opentorah/tree/master/docbook

I moved Mathematics typesetting plugin for FOP into a separate module
(which publishes a separate artifact) so that it can be used for
source formats other than DocBook, but so far nobody did :)

This code also contains integration with Node.js, which can be split into
yet another separate module if the need to use it for running Node.js packages
other than mathjax arises.
