# DocBook plugin for Gradle #

[TOC]
## Motivation ##

There is an excellent [DocBook](http://www.docbook.org/) plugin
for [Maven](https://maven.apache.org/index.html):
[docbkx-tools](https://github.com/mimil/docbkx-tools).
This is my attempt to implement a plugin for [Gradle](https://gradle.org/)
inspired by the ideas pioneered by the Maven plugin. More information about requirements,
motivation and chosen approach can be found in a
[blog post](http://dub.podval.org/2019/05/06/publishing-papers-on-web-2.html).

Plugin supports typesetting mathematics using [MathJax](https://www.mathjax.org/).
To make this possible, integration with [Node.js](https://nodejs.org/en/) was developed.

## Summary ##

Plugin uses [Saxon](https://www.saxonica.com/html/documentation/about/whatis.html)
with DocBook [XSLT stylesheets](https://docbook.org/tools/) to process a DocBook document(s)
(and includes) into HTML, EPUB, EPUB3 and PDF. Processing using DocBook
[XSLT 2.0 stylesheets](https://github.com/docbook/xslt20-stylesheets) is supported for HTML
only ("HTML2"). For PDF, DocBook is first processed into
[XSL-FO](https://www.xml.com/articles/2017/01/01/what-is-xsl-fo/), which is post-processed by
[Apache FOP](https://xmlgraphics.apache.org/fop/). For PDF,
[JEuclid](http://jeuclid.sourceforge.net/) or MathJax FOP plugin can be enabled to process mathematics.
Document name(s) and the list of the output formats are configurable; variants with different settings
for the same output format are supported.

XSL parameters can be configured in the `Gradle` build file using `parameters` map.
There are sections (and customization files) for each output format, for variants of a format,
for all HTML-like formats (`htmlCommon`), and for all formats (`common`) (only `html2` applies to `html2`).
Parameters set in a more general section can be overridden in a more specific one.

Values configured as `substitutions` are available within the DocBook documents by
their names. This mechanism can be used to insert things like processing date or the
version of the document into the output.    

If a data generator class is configured, its `main()` method will be executed with
a directory path as a parameter. References in DocBook documents that are prefixed
with `data:` are resolved to files in that directory.

For each output format or its variant, plugin uses three XSL files: main, `-custom` and `-params`.
Main and `-params` files are overwritten by the plugin at each run; `-custom` stylesheet
can be used to override templates that need customization, define attribute sets and to
set parameters (although it seems cleaner to set them in the `Gradle` build file).
Main file imports appropriate official DocBook XSLT stylesheet,
`-param` file and customization files for all applicable sections (from general to specific).
(Since XSL parameters have the value assigned to them last, `-custom`
needs to be imported after plugin sets the values of the parameters; since import
elements must come before all others, plugin sets the parameters in a separate `-param`
XSL file that is imported before `-custom`.) 
Customizations shared between formats are in `common-custom` and `htmlCommon-custom`.

Plugin generates an XML catalog `catalog.xml` and a DTD with all the configured substitutions,
both of which are overwritten at each run; main catalog chains into `catalog-custom.xml`,
which can be used for customizations: additional URI nicknames, entities, etc.

If they do not exist, plugin generates the following files:
- `-custom` XSL
- `catalog-custom.xml`
- FOP configuration file
- CSS stylesheet used for HTML and EPUB
- input document :)  

For DocBook processing, plugin uses the main XSL file for a format and `catalog.xml`, which can
also be configured in an XML editor to replicate the setup that plugin uses in an environment
more suitable than a code editor or an IDE for authoring DocBook.      

Plugin assumes that images referenced in the DocBook files are in the `images` directory, and sets the 
`img.src.path` XSL parameter accordingly. Images should be referenced *without* the `images/` prefix!

Plugin sets some XSL parameters to reasonable default values, which can be seen in the `-param` XSL files
and overridden in the `-custom` ones. Parameters that plugin sets in the main XSL file (`base.dir`, `img.src.path`)
can not be overridden. Plugin also adds some reasonable customizations to the `-custom` XSL files when
generating them. They can be tweaked/disabled as desired.

To prevent generated XSL files (other than the `-custom`) from being checked in, a `.gitignore`
file can be added to the `src/main/xsl` and/or `src/main/xml` directory:
```git
*
!*-custom.xml
!.gitignore
```

Plugin adds to the project Gradle task `processDocBook` that writes configuration files,
substitutions DTD and XML catalog, generates data (if configured),
installs DocBook XSLT stylesheets, Node and MathJax and processes DocBook.

Plugin sets some parameters in the `-param` stylesheet based on the logging level at the time
`processDocBook` task is executed; for example, `chunk.quietly` is set to `yes` unless
logging at `info` level is enabled.


## Credits ##

I want to thank:
- [Cedric Pronzato](https://github.com/mimil) for [Maven DocBook plugin](https://github.com/mimil/docbkx-tools);
- [Norman Walsh](https://nwalsh.com/) for [DocBook](http://www.docbook.org/), [XML Catalogs](http://xmlcatalogs.org/)
   and [XML Resolver](https://xmlresolver.org/);
- [Bob Stayton](http://www.sagehill.net/bobsbio.html) for
[XSLT stylesheets for DocBook](https://github.com/docbook/xslt10-stylesheets)
and a [book]((http://www.sagehill.net/docbookxsl/)) about them;
- [Michael Kay](https://github.com/michaelhkay) for [Saxon 6](http://saxon.sourceforge.net/saxon6.5.5/)
and [Saxon 9](https://www.saxonica.com/documentation/documentation.xml);
- [Apache FOP team](https://xmlgraphics.apache.org/fop/) for Apache FOP;
- [Max Berger](https://github.com/maxberger) for JEuclid and
its [FOP plugin](http://jeuclid.sourceforge.net/jeuclid-fop/);
- [Emmeran Seehuber](https://github.com/rototor) for [updating JEuclid](https://github.com/rototor/jeuclid);
- [MathJax team](https://www.mathjax.org/#about) for [MathJax](https://www.mathjax.org/) and
[MathJax-node](https://github.com/mathjax/MathJax-node);
- [Sten Roger Sandvik](https://github.com/srs) for [Gradle Node plugin](https://github.com/srs/gradle-node-plugin)
  which served as inspiration for the Node support code;
- [Ian Bull](https://github.com/irbull) for [J2V8](https://github.com/eclipsesource/J2V8);
- [Stuart Johnston](https://github.com/sgjohnston) for inspiring variants functionality.


## Applying to a Gradle project ##

Plugin is [published](https://plugins.gradle.org/plugin/org.opentorah.docbook)
on the Gradle Plugin Portal. To apply it to a Gradle project:

```groovy
plugins {
  id 'org.opentorah.docbook' version '0.8.1'
}
```

Plugin needs to be able to resolve DocBook XSLT stylesheets artifacts, so:

```groovy
repositories {
  mavenCentral()
}
```

If there is `classes` task in the project (for instance, Scala or Java plugin is applied),
`processDocBook` task will depend on it.

If project does not contain any code nor applies any core Gradle plugins,
to get basic tasks like "clean" and "build":

```groovy
plugins {
  id 'base'
}
``` 

To make use of the results of the DocBook processing in a directory-name-independent way:
```groovy
  project.sync {
    from processDocBook.outputDirectory
    into '<published directory>'
  }  
```

## Configuration ##

Plugin adds to the project an extension that can be configured using `docBook` closure: 

```groovy
docBook {
  xslt1version = '1.79.1'
  xslt2version = '2.4.3'
  // by default, latest versions of the DocBook XSLT stylesheets are used ("+");
  // above properties can be used to set a specific version 

  document = 'paper'
  documents = [ 'paper', 'paper2' ]
  // .xml extension is assumed
  // for `documents`, final (and intermediate, if any) output is placed under a subdirectory
  // with the same name as the document;
  // if both `document` and `documents` are configured, and one of the `documents` is named
  // the same as one of the configured output formats, effect is undefined :)

  dataGeneratorClass = 'org.sample.stuff.paper.DataGenerator'
  // by default, no data is generated (`dataGeneratorClass` is empty)

  outputFormats = ["html", "pdf", "epub2", "epub3", "html2"]
  // by default, all supported formats except html2 are generated;
  // this property can be overridden on the command line using `-PdocBook.outputFormats="epub3, html"`


  // there is a parameter section for each format,
  // for HTML-using ones (`htmlCommon`), and for all formats (`common`):
  parameters = [
    "common": [
      "toc.section.depth" : "4"            
    ],
    "htmlCommon": [
      "use.id.as.filename": "yes"
    ],
    "pdf" : [
      "symbol.font.family": "DejaVu Sans",
      "body.font.master"  : "12"
    ],
    "pdf-letter" : [
      "paper.type": "USletter"
    ],
    "pdf-a4" : [
      "paper.type": "A4"
    ]
    // if variants are used and one of the `documents` is named the same as one of the configured variants, effect is undefined :)
  ]

  substitutions = [
    "version": project.version       
  ]
  // names get replaced with configured values in DocBook documents

  cssFile = "main"
  // defaults to "docBook"; .css extension is assumed 

  epubEmbeddedFonts = [ "Liberation Sans" ]
  // embedded fonts should be OpenType or WOFF!

  mathJax {
    enabled = true
    // MathML processing for PDF is disabled by default

    useJ2V8 = true
    // use of native J2V8 bindings is disabled by default and
    // likely only works on (some distributions of) Linux  

    font = "TeX"

    extensions = [ ]

    // delimiters for non-MathML math to be processes by MathJax:
    texDelimiter = '$$'
    texInlineDelimiter = '$'
    asciiMathDelimiter = '`'
  }

  jEuclidEnabled = false
  // no more than one of `jEuclidEnabled` and `mathJax.enabled` can be `true` 
}

docBook.parameters.html2 = [
  "use.id.as.filename": "yes"
]

```

Default variant of an output format (e.g., `pdf`) is processed only if there are no named variants configured for the
same output format (e.g. `pdf-a4`).

## Substitutions ##

Values configured via `substitutions` can be accessed in the DocBook files as XML entities with
appropriate names. For instance, if a substitution value is defined for `"version"`, it can be
accessed like this: `... current version: &version; dated...`.

To enable this functionality, DocBook document has to have a Docbook DOCTYPE declaration, even though
DocBook 5.x doesn't really have a DTD :):

```xml
<!DOCTYPE article
  PUBLIC "-//OASIS//DTD DocBook XML V5.0//EN"
  "http://www.oasis-open.org/docbook/xml/5.0/dtd/docbook.dtd">
```
For root element other than `"article"`, DOCTYPE declaration needs to be adjusted accordingly.

Another way to access substitution values is using processing instructions, for instance:
`<?eval version ?>`. This way, `parameters` are also accessible, since restrictions on the
entity names do not apply to the processing instructions. Unlike entities, processing instructions
can only be used inside XML elements, not in attribute values. Also, undefined entities break the
transformation, while undefined processing instructions are ignored. Processing instructions handling
is internal to the plugin, while entities resolution is externalized using generated substitutions DTD
and XML catalog, making it possible to reproduce entities substitution in an XML editor. 

Special substitutions -  `author`, `title`, `subject`, `keywords`, `creationDate` - are set as the PDF
document's metadata properties (if they are configured); `creationDate` is expected to be formatted as
`(new java.util.Date()).toString()` is.


Both substitutions and parameters are also accessible in the CSS files, for instance:  
```css
@font-face {
  font-family: "@body.font.family@";
  src: url("@body-font-family-url@");
}

body {
  font-family: "@body-font-family@", sans-serif;
}
```

## Fonts ##

Default FOP configuration created by the plugin causes FOP to auto-detect available fonts,
which are then cached by FOP to save time in the future runs. After installing new fonts, FOP's font cache file
needs to be removed for them to be detected.

FOP v2.4 can't use some popular fonts like Roboto and NotoSans, and logs an error
"coverage set class table not yet supported" during fonts auto-detection
(see https://issues.apache.org/jira/browse/FOP-2704 for more details);
I had to use fonts that work well enough and support Latin, Russian and Hebrew scripts: DejaVu and Liberation.
This is no longer an issue in FOP v2.5.

Property `epubEmbeddedFonts` configures font families that should be embedded in EPUB files.

Plugin adds a Gradle task `listFopFonts` that can be used to list all the fonts that *are* available to FOP.

Plugin adds a Gradle task `deleteFopFontsCache` that can be used to delete FOP fonts cache.

## Mathematics ##

Plugin supports typesetting mathematics using MathJax for HTML and EPUB and
either server-side MathJax or JEuclid for PDF. JEuclid typesets in document's fonts; MathJax does not.
MathJax delivers better quality and is under active development; JEuclid is not.
JEuclid can handle MathML; MathJax can handle MathML, TeX, inline TeX and AsciiMath.

## Data ##

Data generation class doesn't have to reside in the same project where DocBook plugin is configured;
in such a case, its artifact needs to be added as a `runtimeOnly` dependency of the main sourceset.
If it is produced by another module of the same multi-module project, `processDocBook` task should depend
on the `assemble` task of that module. 

## Oxygen ##

Plugin's setup should be reproducible in an XML editor like [Oxygen](https://www.oxygenxml.com/):
- run Gradle task `processDocBook`;
- add a project-specific XML catalog `src/main/xml/catalog.xml` in
  Options | Preferences | XML | XML Catalog (check `Project Options`, not `Global Options`);
- use main format-specific XSL file from `src/main/xsl` (e.g., `html.xsl`) to configure transformation scenario; 
- define `img.src.path` parameter as `../images`.


## Directory Layout ##

Overview of the directory layout used by the plugin:

```text
   src/main/
     css/docBook.css
     docBook/<documentName>.xml
     fop/fop.xconf
     images/
     xsl/
       html.xsl
       html-custom.xsl
       html-param.xsl
       ...
     xml/
       catalog.xml
       catalog-custom.xml
       substitutions.dtd  

   build/docBook/
     epub2/<documentName>.epub
     epub3/<documentName>.epub
     html/
       css/
       images/
       index.html
     pdf/<documentName>.pdf
     <documents 1>/
       epub2/<documents 1>.epub
       epub3/<documents 1>.epub
       html/...
       pdf/<documents 1>.pdf
     pdf-<variant 1>/
       ...
     ...  

   build/docBookTmp/
     data/
     epub/
     pdf/
     <document 1>/
       data/
       epub/
       pdf/
     pdf-<variant 1>/
       ...
     ...  
```


### Sources ###

Sources (under `src/main`) contain:
- DocBook document to be processed - `docBook/<documentName>.xml`;
- additional DocBook sources included by the main document - in `docBook/`;
- images used in the DocBook files - in `images/`;
- CSS stylesheet that is used for HTML and EPUB - in `css/docBook.css`;
- additional CSS files imported by the main one - in `css/`  
- DocBook XSLT customizations (specific to the output format or a variant) - in `xsl/html.xsl` and the like;
- FOP configuration - in `fop/fop.xconf`;
- XML catalog and substitutions DTD - in `xml/catalog.xml`, `xml/catalog-custom.xml` and `xml/substitutions.dtd`;

Plugin will create CSS stylesheet, XSL customizations, XML catalog customization and FOP configuration file if
they are not present.  

### Output ###

Final output of the plugin is deposited under `build/docBook`,
in a separate directory for each output format:
- chunked HTML - in `epub/html`
- PDF - in `pdf/<documentName>.pdf`;
- EPUB file - in `epub/<documentName>.epub`.

For documents listed in the `documents` property, final output can be found under a subdirectory
with the same name as the document.

For HTML and EPUB, CSS stylesheets and images are included in the output.
For `html2`, main output file will be called `index.html` *only* if main input file has `xml:id="index"`
on the root element *and* `use.id.as.filename` parameter is set (plugin sets it by default).


### Build ###

Plugin unpacks official DocBook XSLT 1.0 stylesheets under `build/docBookXsl/` and XSLT 2.0 stylesheets
under `build/docBookXsl2/`.
References to the stylesheets are resolved to the local copies, suppressing retrieval of the stylesheets for each build.
Gradle will retrieve them once when resolving dependency added by the plugin - and cache the JAR;
unpacking after each `clean` is cheap.

Data generated by the data generator resides under `build/data`. References to the generated data
encountered in the DocBook documents are resolved to files in that directory. 

For output formats that require post-processing or packing, intermediate output is under `build/docBookTmp`.
For documents listed in the `documents` property, intermediate output is placed under a subdirectory with
the same name as the document.

### Frameworks ###

Frameworks used by the plugin are cached under `~/.gradle.
Distribution for each version of the framework is unpacked into its own folder:
- DocBook XSLT 1.0 and XSLT 2.0 stylesheets - under `docbook`;
- Node.js - under `nodejs`, with modules in `node_modules` subfolder;
- J2V8 library - under `j2v8library`.
With this caching, build time is significantly shorter than when the frameworks are
unpacked under the project's `build` directory, which gets wiped out with every `./gradlew clean`. 

## Past ##

Following features of the Maven Gradle plugin are not supported:
- non-customized XSL
- resolve XSL files based on the type of processing 
- expressions in `<?eval?>`
- access to the project and its properties in `<?eval?>`
- multiple documents with different parameters

## Future ##

I should look into alternative ways of integrating with Node:
[GraalVM](https://www.graalvm.org/) and
[Rhino](https://github.com/mozilla/rhino) / [Trireme](https://github.com/apigee/trireme).

Dyno?

Support for J2V8 should probably be dropped...

## Unlikely Future ##

Here are some issues and potential enhancements that probably won't happen:)

- [ ] add DocBook code highlighting to the pipeline;

### HTML2 ###
- [ ] does "Chapter" suppression work for html2?
- [ ] do MathJax configuration for html2 as for html.
- [ ] how to control numbering with `db` in `autolabel.elements`?
- [ ] how to get navigational links?
- [ ] how to chunk glossary and bibliography?

### JSON ###
- [ ] print JSON prettier using Paiges... :)
- [ ] try using it when printing MathJax configuration -
  which is a value of an XSL parameter, so is formatted as a part of XML document...
- [ ] quotes in JSON for the MathJaX configuration are quoted - 
  can I avoid this double-quoting? Should I?

### Oxygen ###
- [ ] is there any combination of img.src.path, relative URLs, base etc. that makes images *visible*
  in Oxygen while editing - without breaking the plugin?
- [ ] Oxygen's PDF transform fails: doesn't recognize 'data:',
  but in Author view works fine... resolver instead of the xmlresolver?
- [ ] add note to README.md about FOP in Oxygen being obsolete - or not supporting JEuclid

### EPUB ###
- [ ] DocBook XSLT stylesheets for EPUB2 do not add the mimetype file with "application/epub+zip" in it; write it from the plugin itself?
- [ ] codify EPUB customization along the lines of [ChunkingCustomization](http://www.sagehill.net/docbookxsl/ChunkingCustomization.html)
  and epub3/README from the XSLT stlesheets distribution;
- [ ] should CSS be configured for EPUB?
