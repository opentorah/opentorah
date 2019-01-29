# Using the Plugin #

## Directory Layout ##

    // Directory layout:
    // Sources:
    //   src/main/
    //     css/
    //     docBook/
    //     fop/fop.xconf
    //     images/
    //     xsl/
    //       epub.xsl
    //       fo.xsl
    //       html.xsl
    //
    // Build:
    //   build/
    //     data/
    //     docBookXsl/docbook/
    //     epub-expanded/
    //
    // Output:
    //   build/docBook/
    //     epub/<inputFileName>.epub
    //     html/
    //       css/
    //       images/
    //       index.html
    //     pdf/<inputFileName>.pdf


Docbook sources and assets are under `src/main`: 
  docbook/
  images/
images should be referenced by name, without the `images/` prefix;  
  css/

When configuring "html.stylesheet" XSLT parameter, use the `css/` prefix: e.g., `css/docbook.css`  

## Customizations ##

  fop/
  xsl/  

## Extension ##

## Parameters ##

Plugin's Saxon tasks set some parameters in accordance with the expected directory layout:
"img.src.path" is set to "images/" and "html.stylesheet" is set to "css/docbook.xsl".
Those parameters are set by the plugin only if they are not set in the DocBook extension's
"parameters" setting, and it's best to leave them alone :)  



## Tasks ##

## Plugin TODO ##

- [ ] Roboto and NotoSans fonts do not work with FOP: https://issues.apache.org/jira/browse/FOP-2704
      ("coverage set class table not yet supported"); what fonts CAN I use?
- [ ] track down and suppress network IO (pre-load net.sf.docbook:docbook-xml:5.0-all?)
- [ ] remove from XSL files' headers: xmlns:xs="http://www.w3.org/2001/XMLSchema"  exclude-result-prefixes="xs"
- [ ] figure out how to define XML entities or processing instructions like <?eval ${project.version}?> programmatically for things like version;
- [ ] what is missing for feature-parity with docbkx?

- [ ] verify that Oxygen can be configured to resolve DocBook XSL and data URLs;
- [ ] see if IntelliJ Idea *and* Oxygen can handle substitution tokens;

- [ ] turn Rant.md into an updated version of the "Publishing on the Web" blog post;
- [ ] add README.md to Bintray upload
- [ ] why aren't tags etc. uploaded to Bintray?
- [ ] expose on Gradle plugin portal
- [ ] use DirectoryProperty/FileProperty
- [ ] add docBookProcess.outputDirectory property and use it in publishing
