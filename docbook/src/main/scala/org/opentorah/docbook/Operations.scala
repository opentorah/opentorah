package org.opentorah.docbook

import org.opentorah.docbook.section.{CommonSection, DocBook2, NonOverridableParameters, Sections, Variant}
import org.opentorah.fop.{Fop, FopPlugin, JEuclidFopPlugin, MathJaxRunner, MathJaxFopPlugin}
import org.opentorah.mathjax.MathJaxConfiguration
import org.opentorah.util.Files
import org.opentorah.xml.{Catalog, Doctype, EvalFilter, PrettyPrinter, Resolver, Sax, Saxon, XInclude, Xml}

import java.io.{File, FileWriter}
import scala.xml.Comment

object Operations {

  def write(
    file: File,
    replace: Boolean,
    doctype: Option[Doctype] = None,
    elem: Xml.Element
  ): Unit = Files.write(
    file,
    replace,
    content = PrettyPrinter.default.renderXml(element = elem, doctype)
  )

  def writeFopConfigurationFile(layout: Layout): Unit = write(
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

  def writeInputFile(layout: Layout, inputFileName: String): Unit = write(
    file = layout.inputFile(inputFileName),
    replace = false,
    doctype = Some(DocBook),
    elem =
        <article xmlns={DocBook.namespace.uri} version={DocBook.version} xmlns:xi={XInclude.namespace.uri}/>
  )

  def writeCatalog(
    layout: Layout,
    xslt1: Option[File],
    xslt2: Option[File],
    substitutions: Map[String, String]
  ): Resolver = {
    val catalogFile: File = layout.catalogFile

    // Substitutions DTD
    Files.write(
      file = layout.xmlFile(layout.substitutionsDtdFileName),
      replace = true,
      content = substitutions.toSeq.map {
        case (name: String, value: String) => s"""<!ENTITY $name "$value">\n"""
      }.mkString
    )

    // Custom XML catalog
    write(
      file = layout.xmlFile(layout.catalogCustomFileName),
      replace = false,
      doctype = Some(Catalog),
      elem =
        <catalog xmlns={Catalog.namespace.uri} prefer="public">
          <!-- Customizations go here. -->
          <nextCatalog catalog="/etc/xml/catalog"/>
        </catalog>
    )

    // XML catalog
    write(
      file = catalogFile,
      replace = true,
      doctype = Some(Catalog),
      elem =
        <catalog xmlns={Catalog.namespace.uri} prefer="public">
          {Comment(s" DO NOT EDIT! Generated by the DocBook plugin. Customizations go into ${layout.catalogCustomFileName}. ")}
          <group xml:base={layout.catalogGroupBase}>
            <!--
            There seems to be some confusion with the rewriteURI form:
            Catalog DTD requires 'uriIdStartString' attribute (and that is what IntelliJ wants),
            but XMLResolver looks for the 'uriStartString' attribute (and this seems to work in Oxygen).
            -->

            {xslt1.toSeq.map(_     => <!-- DocBook XSLT 1.0 stylesheets  -->)}
            {xslt1.toSeq.map(xslt1 => <rewriteURI uriStartString={Stylesheets.xslt1.uri} rewritePrefix={s"$xslt1/"}/>)}

            {xslt2.toSeq.map(_     => <!-- DocBook XSLT 2.0 stylesheets  -->)}
            {xslt2.toSeq.map(xslt2 => <rewriteURI uriStartString={Stylesheets.xslt2.uri} rewritePrefix={s"$xslt2/"}/>)}

            <!-- generated data -->
            <rewriteSystem rewritePrefix={layout.dataDirectoryRelative} systemIdStartString="data:/"/>
            <rewriteSystem rewritePrefix={layout.dataDirectoryRelative} systemIdStartString="data:"/>
            <rewriteSystem rewritePrefix={layout.dataDirectoryRelative} systemIdStartString="urn:docbook:data:/"/>
            <rewriteSystem rewritePrefix={layout.dataDirectoryRelative} systemIdStartString="urn:docbook:data:"/>
            <rewriteSystem rewritePrefix={layout.dataDirectoryRelative} systemIdStartString="urn:docbook:data/"/>
            <rewriteSystem rewritePrefix={layout.dataDirectoryRelative} systemIdStartString="http://opentorah.org/docbook/data/"/>
          </group>

          <!-- substitutions DTD -->
          <public publicId={DocBook.dtdId} uri={layout.substitutionsDtdFileName}/>

          <nextCatalog catalog={layout.catalogCustomFileName}/>
        </catalog>
    )

    new Resolver(catalogFile)
  }

  def writeStylesheets(
    layout: Layout,
    sections: Sections,
    inputDocuments: List[(String, Boolean)]  ,
    isInfoEnabled: Boolean,
    embeddedFonts: String,
    cssFileName: String,
    useMathJax3: Boolean,
    mathJaxConfiguration: MathJaxConfiguration,
    enableMathJax: Boolean
  ): Unit = {
    // Custom stylesheet
    for (section: CommonSection <- CommonSection.all) Operations.write(
      file = layout.stylesheetFile(layout.customStylesheet(section)),
      replace = false,
      elem = section.customStylesheet
    )
    for (variant: Variant <- sections.allVariants) Operations.write(
      file = layout.stylesheetFile(layout.customStylesheet(variant)),
      replace = false,
      elem = variant.docBook2.customStylesheet
    )

    // Parameters stylesheet
    for (variant: Variant <- sections.allVariants) Operations.write(
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
      useMathJax3 = useMathJax3,
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
    useMathJax3: Boolean,
    mathJaxConfiguration: MathJaxConfiguration,
    enableMathJax: Boolean
  ): Unit = {
    val forDocument: Layout.ForDocument = layout.forDocument(prefixed, documentName)

    // xsl:param has the last value assigned to it, so customization must come last;
    // since it is imported (so as not to be overwritten), and import elements must come first,
    // a separate "-param" file is written with the "default" values for the parameters :)
    write(
      file = layout.stylesheetFile(forDocument.mainStylesheet(variant)),
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
          useMathJax3 = useMathJax3,
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
