package org.opentorah.docbook

import org.opentorah.docbook.section.{CommonSection, DocBook2, NonOverridableParameters, Sections, Variant}
import org.opentorah.fop.{Fop, FopPlugin, JEuclidFopPlugin, MathJaxFopPlugin, MathJaxRunner}
import org.opentorah.mathjax.{MathJax, MathJaxConfiguration}
import org.opentorah.util.Files
import org.opentorah.xml.{Catalog, EvalFilter, PrettyPrinter, Resolver, Sax, Saxon, XInclude, Xml}
import java.io.{File, FileWriter}

object Operations {

  def writeFopConfigurationFile(layout: Layout): Unit = PrettyPrinter.default.write(
    file = layout.fopConfigurationFile,
    replace = false,
    elem =
      <fop version="1.0">
        <renderers>
          <renderer mime="application/pdf">
            <fonts>
              <!-- FOP will detect fonts available in the operating system. -->
              <auto-detect/>
            </fonts>
          </renderer>
        </renderers>
      </fop>
  )

  def writeCssFile(layout: Layout, cssFileName: String): Unit = Files.write(
    file = layout.cssFile(cssFileName),
    replace = false,
    content =
      s"""@namespace xml "${Xml.namespace.uri}";
         |""".stripMargin
  )

  def writeInputDocuments(layout: Layout, inputDocuments: List[(String, Boolean)]): Unit =
    for ((documentName: String, prefixed: Boolean) <- inputDocuments) PrettyPrinter.default.write(
      file = layout.forDocument(prefixed, documentName).inputFile,
      replace = false,
      doctype = Some(DocBook),
      elem =
          <article xmlns={DocBook.namespace.uri} version={DocBook.version} xmlns:xi={XInclude.namespace.uri}/>
    )

  private def xsltUri(stylesheets: Stylesheets, directory: File): Seq[Xml.Node] = Seq(
    Xml.mkComment(s" DocBook ${stylesheets.name} stylesheets "),
    Catalog.rewriteUri(rewritePrefix = s"$directory/", uriStartString = s"${stylesheets.uri}/")
  )

  def writeCatalog(
    layout: Layout,
    xslt1: File,
    xslt2: File,
    substitutions: Map[String, String]
  ): Unit = {
    val catalogFile: File = layout.catalogFile
    val customCatalogFile: File = layout.customCatalogFile
    val dtdFile: File = layout.dtdFile
    val dataDirectory: File = layout.dataDirectory

    DocBook.writeDtd(dtdFile, substitutions)

    // Custom XML catalog
    Catalog.write(
      file = customCatalogFile,
      replace = false,
      content = Seq(
        Xml.mkComment(" Customizations go here. "),
        Catalog.nextCatalogSystem
      )
    )

    // XML catalog
    Catalog.write(
      file = catalogFile,
      replace = true,
      content = Seq(
        Xml.mkComment(s" DO NOT EDIT! Generated by the DocBook plugin. "),
        Xml.mkComment(s" customizations go into $customCatalogFile. "),
        Catalog.nextCatalog(customCatalogFile.getPath),

        Xml.mkComment(" substitutions DTD "),
        DocBook.dtdLink(dtdFile),

        Xml.mkComment(" generated data ")
      ) ++
      DocBook.data(dataDirectory) ++

      xsltUri(Stylesheets.xslt1, xslt1) ++
      xsltUri(Stylesheets.xslt2, xslt2)
    )
  }

  def writeStylesheets(
    layout: Layout,
    sections: Sections,
    inputDocuments: List[(String, Boolean)],
    isInfoEnabled: Boolean,
    embeddedFonts: String,
    cssFileName: String,
    mathJax: MathJax,
    mathJaxConfiguration: MathJaxConfiguration,
    enableMathJax: Boolean
  ): Unit = {
    // Custom stylesheet
    for (section: CommonSection <- CommonSection.all) PrettyPrinter.default.write(
      file = layout.stylesheetFile(layout.customStylesheet(section)),
      replace = false,
      elem = section.customStylesheet
    )
    for (variant: Variant <- sections.allVariants) PrettyPrinter.default.write(
      file = layout.stylesheetFile(layout.customStylesheet(variant)),
      replace = false,
      elem = variant.docBook2.customStylesheet
    )

    // Parameters stylesheet
    for (variant: Variant <- sections.allVariants) PrettyPrinter.default.write(
      file = layout.stylesheetFile(layout.paramsStylesheet(variant)),
      replace = true,
      elem = variant.docBook2.paramsStylesheet(sections.parameters(variant))
    )

    // Main stylesheet
    for {
      variant: Variant <- sections.allVariants
      (documentName: String, prefixed: Boolean) <- inputDocuments
    } Operations.writeMainStylesheet(
      layout = layout,
      documentName = documentName,
      prefixed = prefixed,
      variant = variant,
      isInfoEnabled = isInfoEnabled,
      embeddedFonts = embeddedFonts,
      cssFileName = cssFileName,
      mathJax = mathJax,
      mathJaxConfiguration = mathJaxConfiguration,
      enableMathJax = enableMathJax
    )
  }

  def writeMainStylesheet(
    layout: Layout,
    documentName: String,
    prefixed: Boolean,
    variant: Variant,
    isInfoEnabled: Boolean,
    embeddedFonts: String,
    cssFileName: String,
    mathJax: MathJax,
    mathJaxConfiguration: MathJaxConfiguration,
    enableMathJax: Boolean
  ): Unit = {
    val forDocument: Layout.ForDocument = layout.forDocument(prefixed, documentName)

    // xsl:param has the last value assigned to it, so customization must come last;
    // since it is imported (so as not to be overwritten), and import elements must come first,
    // a separate "-param" file is written with the "default" values for the parameters :)
    PrettyPrinter.default.write(
      file = forDocument.mainStylesheetFile(variant),
      replace = true,
      elem = variant.docBook2.mainStylesheet(
        paramsStylesheetName = layout.paramsStylesheet(variant),
        stylesheetUriBase = (if (variant.docBook2.usesDocBookXslt2) Stylesheets.xslt2 else Stylesheets.xslt1).uri,
        customStylesheets =
          variant.docBook2.commonSections.map(layout.customStylesheet) ++
            (variant.baseVariant.toSeq :+ variant).map(layout.customStylesheet),
        values = new NonOverridableParameters(
          isInfoEnabled = isInfoEnabled,
          embeddedFonts = embeddedFonts,
          cssFile = layout.cssFileRelativeToOutputDirectory(cssFileName),
          imagesDirectoryName = layout.imagesDirectoryName,
          mathJax = mathJax,
          mathJaxConfiguration = if(!enableMathJax) None else Some(mathJaxConfiguration),
          documentName = documentName,
          saxonOutputDirectory = forDocument.saxonOutputDirectory(variant)
        )
      )
    )
  }

  def runSaxon(
    docBook2: DocBook2,
    inputFile: File,
    substitutions: Map[String, String],
    mathJaxRunner: Option[MathJaxRunner],
    resolver: Resolver,
    stylesheetFile: File,
    saxonOutputFile: File
  ): Unit = {
    // Run Saxon.
    // Note: DocBook XSLT uses Saxon 6 XSLT 1.0 extensions and doesn't work on later Saxon versions
    // ("Don't know how to chunk with Saxonica").
    // According to https://www.saxonica.com/html/documentation/extensions/instructions/output.html,
    //   "Saxon 9.9 reintroduces saxon6:output (in the original Saxon 6.5.5 namespace,
    //   which differs from the usual Saxon namespace, so here we use a different prefix)
    //   so that the DocBook 1.0 stylesheets can now be executed with a modern Saxon release.
    //   Note that the specification is not identical with the Saxon 6.5.5 original,
    //   but it serves the purpose in supporting DocBook."
    // I am not sure what I can do to set up DocBook XSLT 1 processing with Saxon 10
    // (it didn't work out of the box for me), but I'd love to get rid of the Saxon 6, since it:
    // - produces unmodifiable DOM (see Saxon) - unlike Saxon 10,
    // - carries within it obsolete org.w3c.dom classes (Level 2), which cause IDE to highlight
    //   as errors uses of the (Level 3) method org.w3c.dom.Node.getTextContent()...
    val saxon: Saxon = if (!docBook2.usesDocBookXslt2) Saxon.Saxon6 else Saxon.Saxon10

    // do not output the 'main' file when chunking in XSLT 1.0
    val result: javax.xml.transform.stream.StreamResult = new javax.xml.transform.stream.StreamResult
    if (docBook2.usesRootFile) {
      result.setSystemId(saxonOutputFile)
      result.setWriter(new FileWriter(saxonOutputFile))
    } else {
      result.setSystemId("dev-null")
      result.setOutputStream((_: Int) => {})
    }

    saxon.transform(
      filters = Seq(new EvalFilter(substitutions)) ++
        (if (mathJaxRunner.isDefined && docBook2.isPdf) Seq(new MathFilter(mathJaxRunner.get.configuration)) else Seq.empty),
      // ++ Seq(new TracingFilter),
      resolver = Some(resolver),
      stylesheetFile = Some(stylesheetFile),
      inputSource = Sax.file2inputSource(inputFile),
      result = result
    )
  }

  def postProcess(
    layout: Layout,
    docBook2: DocBook2,
    outputFile: File,
    saxonOutputDirectory: File,
    saxonOutputFile: File,
    substitutions: Map[String, String],
    isJEuclidEnabled: Boolean,
    mathJaxRunner: Option[MathJaxRunner]
  ): Unit = {
    if (docBook2.isPdf) {
      val fopPlugin: Option[FopPlugin] =
        if (isJEuclidEnabled) Some(new JEuclidFopPlugin)
        else mathJaxRunner.map(new MathJaxFopPlugin(_))

      Fop.run(
        saxon = Saxon.Saxon10,
        configurationFile = layout.fopConfigurationFile,
        creationDate = substitutions.get("creationDate"),
        author = substitutions.get("author"),
        title = substitutions.get("title"),
        subject = substitutions.get("subject"),
        keywords = substitutions.get("keywords"),
        inputFile = saxonOutputFile,
        outputFile,
        fopPlugin,
      )
    }

    docBook2.postProcess(
      inputDirectory = saxonOutputDirectory,
      outputFile
    )
  }
}
