# DocBook plugin for Gradle #

## Motivation ##

There is an excellent [DocBook](http://www.docbook.org/) plugin
for [Maven](https://maven.apache.org/index.html):
[docbkx-tools](https://github.com/mimil/docbkx-tools).
For [Gradle](https://gradle.org/), I found some Groovy scripts floating around,
but no general-purpose plugins.

This is my attempt at one, inspired by the ideas pioneered by the Maven plugin.
Thank you, [@mimil](https://github.com/mimil)!


## Theory of Operation ##

The plugin uses Saxon with DocBook XSLT stylesheets to transform DocBook documents into
HTML, EPUB and XSL-FO, and Apache FOP to transform XSL-FO into PDF.


## Adding to Gradle project ##

To add the plugin to your Gradle project:

```groovy
buildscript {
  // for the plugin
  repositories {
    jcenter()
  }

  dependencies {
    classpath 'org.podval.docbook:org.podval.docbook-gradle-plugin:+'
  }
}

// for DocBook XSLT
repositories {
  jcenter()
}

apply plugin: 'org.podval.docbook-gradle-plugin'
```

For projects with code that is used to generate data for inclusion in the DocBook files,
DocBook plugin needs to be applied *after* the Java plugin.

If project does not contain any code nor applies any core Gradle plugins,
to get basic tasks like "clean" and "build":

```groovy
apply plugin: 'base'
``` 

## Directory Layout ##

Overview of the directory layout used by the plugin:

```
   src/main/
     css/docBook.css
     docBook/<documentName>.xml
     fop/fop.xconf
     images/
     xsl/
       epub.xsl
       fo.xsl
       html.xsl

   build/docBook/
     epub/<documentName>.epub
     html/
       css/
       images/
       index.html
     pdf/<documentName>.pdf

   build/docBookXsl/docbook/

   build/docBookTmp/
     data/
     epub/
     fo/
```

### Sources ###

Sources (under `src/main`) contain:
- DocBook source of the document to be processed - `docBook/<documentName>.xml`;
- additional DocBook sources included by the main document - in `docBook/`;
- images used in the DocBook files - in `images/`;
- CSS stylesheet that is used for HTML and EPUB - in `css/docBook.css`;
- additional CSS files imported by the main one - in `css/`  
- DocBook XSLT customizations (specific to the output format) - in `xsl/html.xsl` and the like;
- FOP configuration - in /fop/fop.xconf.

Plugin will create CSS stylesheet, XSL customizations and FOP configuration files if
they are not present.  

### Build ###

Plugin ....

### Output ###

Final output of the plugin is deposited under `build/docBook`,
in a separate directory for each output format:
- chunked HTML - in `epub/html`
- PDF - in `pdf/<documentName>.pdf`;
- EPUB file - in `epub/<documentName>.epub`.

For HTML and EPUB, CSS stylesheets and images are included in the output.

## Customizations ##

  fop/
  xsl/  

Some configuration that could be placed in the XSL files was placed in the POM:
img.src.path: to be near the related - copying of the images
font configuration: to centralize configuration and avoid duplication (filtering is used to patch font configuration into CSS files)


## Extension ##

Plugin adds to the project an extension that can be configured using `docBook` closure: 

```groovy
docBook {
  documentName = "calendar"

  dataGeneratorClass = "org.podval.calendar.paper.Tables"

  parameters = [
    "body.font.family": "DejaVu Sans",
    "body.font.master": "12"
  ]
}
```

## Parameters ##

Plugin's Saxon tasks set some parameters in accordance with the expected directory layout:
"img.src.path" is set to "images/" and "html.stylesheet" is set to "css/docbook.xsl".
Those parameters are set by the plugin only if they are not set in the DocBook extension's
"parameters" setting, and it's best to leave them alone :)  

## Images ##

Plugin sets `img.src.path` XSLT parameter, so images should be referenced in the DocBook
files *without* the `images/` prefix!

## Fonts ##

Default FOP configuration created by the plugin causes FOP to auto-detect available fonts,
which are then cached by FOP to save time. After installing new fonts, FOP's font cache file
needs to be removed for them to be detected.

FOP can't use some popular fonts like Roboto and NotoSans, and logs an error
"coverage set class table not yet supported" during fonts auto-detection;
see https://issues.apache.org/jira/browse/FOP-2704 for more details.

Plugin adds a Gradle task that can be used to list all the fonts that *are* available to FOP:
```
./gradlew listFonts
```

Some of the fonts that work well enough and support Latin, Russian and Hebrew scripts
are DejaVu and Liberation.

Parameters set in XSLT customizations can be used to set fonts for the PDF output.
Parameters set in the `docBook` closure of the `Gradle` build file can be used to set fonts both for the PDF output
and for CSS, since they are substituted in the CSS files: 
```css
@font-face {
  font-family: "@body.font.family@";
  src: url("@body.font.family.url@");
}

body {
  font-family: "@body-font-family@", sans-serif;
}
```

## Tasks ##

