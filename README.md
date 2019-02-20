# DocBook plugin for Gradle #

## Motivation ##

There is an excellent [DocBook](http://www.docbook.org/) plugin
for [Maven](https://maven.apache.org/index.html):
[docbkx-tools](https://github.com/mimil/docbkx-tools).
For [Gradle](https://gradle.org/), I found some Groovy scripts floating around,
but no general-purpose plugins. This is my attempt at one, inspired by the ideas
pioneered by the Maven plugin.


## Summary ##

Plugin uses Saxon with DocBook XSLT stylesheets to process a DocBook document
(and its includes) into HTML, EPUB, EPUB3 and PDF. Processing using XSLT 2.0
DocBook stylesheets is supported for HTML only ("html2"). For PDF, DocBook is
first processed into XSL-FO, which is post-processed by Apache FOP. For PDF,
JEuclid FOP plugin can be enabled to process MathML. Document name and the list
of the output formats are configurable.

XSL parameters can be configured in the `Gradle` build file usin `parameters` map,
which can have sections applicable to a specific format (`html`), all HTML-like
formats (`htmlCommon`), and for all formats except `html2` (`common`).

Values configured as `substitutions` are available within the DocBook documents by
their names. This mechanism can be used to insert things like processing date or the
version of the document into the output.    

If a data generator class is configured, it's `main()` method will be executed with
a directory path as a parameter. References in DocBook documents that are prefixed
with `data:` are resolved to files in that directory.

For each output format, plugin uses three XSL files: main, `-custom` and `-params`.
Main and `-params` files are overwritten by the plugin at each run; `-custom` is
where customizations go. Main file imports appropriate official DocBook XSLT stylesheet -
and the other two files. (Since XSL parameters have the value assigned to them last, `-custom`
needs to be imported after plugin sets the values of the parameters; since import
elements must come before all others, plugin sets the parameters in a separate `-param`
XSL file that is imported before `-custom`.)

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


## Credits ##

I want to thank:
- [@mimil](https://github.com/mimil) for the [Maven DocBook plugin](https://github.com/mimil/docbkx-tools);
- [Norman Walsh](https://nwalsh.com/) for his work on [DocBook](http://www.docbook.org/),
[XML Catalogs](http://xmlcatalogs.org/) and [XML Resolver](https://xmlresolver.org/);
- [Bob Stayton](http://www.sagehill.net/bobsbio.html) for
[XSLT stylesheets for DocBook](https://github.com/docbook/xslt10-stylesheets)
and a [book]((http://www.sagehill.net/docbookxsl/)) about them;
- [Michael Kay](https://github.com/michaelhkay) for [Saxon 6](http://saxon.sourceforge.net/saxon6.5.5/)
and [Saxon 9](https://www.saxonica.com/documentation/documentation.xml);
- [Apache FOP team](https://xmlgraphics.apache.org/fop/) for Apache FOP;
- [Max Berger](https://github.com/maxberger) for the original work on JEuclid and
its [FOP plugin](http://jeuclid.sourceforge.net/jeuclid-fop/);
- [Emmeran Seehuber](https://github.com/rototor) for [updating JEuclid](https://github.com/rototor/jeuclid).


## Applying to a Gradle project ##

Plugin is [published](https://plugins.gradle.org/plugin/org.podval.docbook-gradle-plugin)
on the Gradle Plugin Portal. To apply it to a Gradle project:

```groovy
plugins {
  id "org.podval.docbook-gradle-plugin" version "0.6.6"
}
```

Plugin needs to be able to resolve DocBook XSLT stylesheets artifacts, so:

```groovy
repositories {
  jcenter()
}
```

For projects with code that is used to generate data for inclusion in the DocBook files,
DocBook plugin needs to be applied *after* the Scala/Java plugin - or explicit dependency needs
to be added by hand:

```groovy
prepareDocBook.dependsOn classes
```

If project does not contain any code nor applies any core Gradle plugins,
to get basic tasks like "clean" and "build":

```groovy
apply plugin: 'base'
``` 

Plugin adds to the project Gradle task `prepareDocBook` that writes configuration files,
substitutions DTD and XML catalog, generates data (if configured) and unpacks DocBook XSLT stylesheets
 
Plugin adds to the project Gradle task `processDocBook` that processes DocBook :)

To make use of the DocBook processing in a directory-name-independent way:
```groovy
  project.sync {
    from project.tasks.getByPath(':<subproject with DocBook sources>:processDocBook').outputDirectory
    into '<published directory>'
  }  
```


## Configuration ##

Plugin adds to the project an extension that can be configured using `docBook` closure: 

```groovy
docBook {
  xslt1version = "1.79.1"
  xslt2version = "2.3.10"
  // by default, latest versions of the DocBook XSLT stylesheets are used ("+");
  // above properties can be used to set a specific version 

  documentName = "paper"
  // defaults to "index" (.xml is assumed)

  dataGeneratorClass = "org.sample.stuff.paper.Tables"
  // by default, no data is generated 

  outputFormats = ["html", "pdf", "epub2", "epub3", "html2"]
  // by default, all supported formats except html2 are generated;
  // this property can be overridden on the command line using `-PdocBook.outputFormats="epub3, html"`

  parameters = [
    "common": [
      "toc.section.depth": "4"            
    ],
    "htmlCommon": [
      "use.id.as.filename": "yes"
    ],
    "pdf" : [
      "body.font.master": "12"
    ]
  ]

  substitutions = [
    "version": project.version       
  ]
  // names get replaced with configured values in DocBook documents

  cssFileName = "main"
  // defaults to "docBook" (.css is assumed) 

  epubEmbeddedFonts = [ "Liberation Sans" ]
  // embedded fonts should be OpenType or WOFF!

  isJEuclidEnabled = true
  // MathML processing for PDF is disabled by default 
}

docBook.parameters.html2 = [
  "use.id.as.filename": "yes"
]

```


### substitutions ###

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

## Images ##

Plugin uses images from the `images` directory, and sets the `img.src.path` XSL parameter accordingly.
Images should be referenced in the DocBook files *without* the `images/` prefix!


## Fonts ##

Default FOP configuration created by the plugin causes FOP to auto-detect available fonts,
which are then cached by FOP to save time. After installing new fonts, FOP's font cache file
needs to be removed for them to be detected.

FOP can't use some popular fonts like Roboto and NotoSans, and logs an error
"coverage set class table not yet supported" during fonts auto-detection;
see https://issues.apache.org/jira/browse/FOP-2704 for more details.

Plugin adds a Gradle task `listFonts` that can be used to list all the fonts that *are* available to FOP.

Some of the fonts that work well enough and support Latin, Russian and Hebrew scripts
are DejaVu and Liberation.

Property `epubEmbeddedFonts` configures font families that should be embedded in EPUB files.


## Oxygen ##

Plugin's setup should be reproducible in an XML editor like [Oxygen](https://www.oxygenxml.com/):
- run Gradle task `prepareDocBook`;
- add a project-specific XML catalog `src/main/xml/catalog.xml` in
  Options | Preferences | XML | XML Catalog (check `Project Options`, not `Global Options`);
- use main format-specific XSL file from `src/main/xsl` (e.g., html.xsl) to configure transformation scenario; 
- define `img.src.path` parameter as `../images`.


## Directory Layout ##

Overview of the directory layout used by the plugin:

```
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
     epub/<documentName>.epub
     html/
       css/
       images/
       index.html
     pdf/<documentName>.pdf

   build/docBookXsl/
   build/docBookXsl2/

   build/docBookTmp/
     data/
     epub/
     fo/
```


### Sources ###

Sources (under `src/main`) contain:
- DocBook document to be processed - `docBook/<documentName>.xml`;
- additional DocBook sources included by the main document - in `docBook/`;
- images used in the DocBook files - in `images/`;
- CSS stylesheet that is used for HTML and EPUB - in `css/docBook.css`;
- additional CSS files imported by the main one - in `css/`  
- DocBook XSLT customizations (specific to the output format) - in `xsl/html.xsl` and the like;
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

For HTML and EPUB, CSS stylesheets and images are included in the output.


### Build ###

Plugin unpacks official DocBook XSLT 1.0 stylesheets under `build/docBookXsl/` and XSLT 2.0 stylesheets under `build/docBookXsl2/`.
References to the stylesheets are resolved to the local copies, suppressing retrieval of the stylesheets for each build.
Gradle will retrieve them once when resolving dependency added by the plugin - and cache the JAR;
unpacking after each `clean` is cheap.

Data generated by the data generator resides under `duild/data`. References to the generated data
encountered in the DocBook documents are resolved to files in that directory. 

For output formats that require post-processing or packing, intermediate results are deposited
under `build/docBookTmp`.


## Past ##

Following features of the Maven Gradle plugin are not supported:
- non-customized XSL
- resolve XSL files based on the type of processing 
- expressions in <?eval?>
- access to the project and its properties in <?eval?>
- multiple executions with different documents and parameters


## Future ##

Following enhancements are being considered:
- look into running MatJax stylesheets in the plugin (without the browser) to
  convert MathML for PDF (that way, LaTeX will also be supported);
- look into XSLT solutions for MatML/LaTeX -> SVG conversion
  ([pmml2svg](http://pmml2svg.sourceforge.net/doc/user-xhtml-svg/index.xhtml)
   doesn't seem to be maintained).
