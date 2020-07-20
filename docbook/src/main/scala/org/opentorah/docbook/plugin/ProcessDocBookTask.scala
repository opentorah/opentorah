package org.opentorah.docbook.plugin

import java.io.File
import java.net.URI
import org.gradle.api.logging.Logger
import org.gradle.api.DefaultTask
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.gradle.api.tasks.{Input, Internal, SourceSet, TaskAction}
import org.opentorah.docbook.section.{CommonSection, DocBook2, NonOverridableParameters, Section, Sections, Variant}
import org.opentorah.fop.{Fop, FopFonts}
import org.opentorah.mathjax
import org.opentorah.mathjax.MathJax
import org.opentorah.util.Collections.mapValues
import org.opentorah.util.{Files, Gradle}
import org.opentorah.xml.{Namespace, PrettyPrinter, Resolver}
import scala.beans.BeanProperty
import scala.collection.JavaConverters._
import scala.xml.{Comment, Elem}

class ProcessDocBookTask extends DefaultTask {

  private val layout: Layout = Layout.forProject(getProject)
  private val logger: Logger = getProject.getLogger
  private def info(message: String): Unit = logger.info(message, null, null, null)

  // To let projects that use the plugin to not make assumptions about directory names:
  @Internal def getOutputDirectory: File = layout.outputRoot

  private def getProcessInputDirectories: Set[File] = Set(
    layout.inputDirectory,
    layout.cssDirectory,
    layout.fopConfigurationDirectory,
    layout.stylesheetDirectory
  ) ++ Set(
    layout.imagesDirectory,
  ).filter(_.exists)

  // Register inputs

  getProcessInputDirectories.foreach(registerInputDirectory)

  // Data generation class doesn't have to reside in the same project where DocBook plugin is configured,
  // so I don't bother registering classes as input directory, but if I did:
  // classesDirs use sourceSets, which are only available after project is evaluated;
  // with code in Scala only, input directory "build/classes/java/main" doesn't exist (which is a Gradle error), so I
  // register "build/classes/" (grandparent of all classesDirs) as input directory
  // (is there a simpler way of getting it?).
  //  getProject.afterEvaluate((project: Project) =>
  //    Gradle.classesDirs(project).map(_.getParentFile.getParentFile).foreach(registerInputDirectory))

  private def registerInputDirectory(directory: File): Unit = {
    info(s"processDocBook: registering input directory $directory")
    directory.mkdirs()
    getInputs.dir(directory)
  }

  // Register outputs
  private def getProcessOutputDirectories: Set[File] = Set(
    layout.intermediateRoot,
    layout.outputRoot
  )
  for (directory: File <- getProcessOutputDirectories) {
    // Note: deleting directories makes the task not up-to-date: Files.deleteRecursively(directory)
    info(s"processDocBook: registering output directory $directory")
    getOutputs.dir(directory)
  }

  @Input @BeanProperty val document: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val documents: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @Input @BeanProperty val parameters: MapProperty[String, java.util.Map[String, String]] =
    getProject.getObjects.mapProperty(classOf[String], classOf[java.util.Map[String, String]])

  @Input @BeanProperty val substitutions: MapProperty[String, String] =
    getProject.getObjects.mapProperty(classOf[String], classOf[String])

  @Input @BeanProperty val outputFormats: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @Input @BeanProperty val isMathJaxEnabled: Property[Boolean] =
    getProject.getObjects.property(classOf[Boolean])

  @Input @BeanProperty val useJ2V8: Property[Boolean] =
    getProject.getObjects.property(classOf[Boolean])

  @Input @BeanProperty val mathJaxFont: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val mathJaxExtensions: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @Input @BeanProperty val texDelimiter: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val texInlineDelimiter: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val asciiMathDelimiter: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val isJEuclidEnabled: Property[Boolean] =
    getProject.getObjects.property(classOf[Boolean])

  @Input @BeanProperty val xslt1version: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val xslt2version: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val nodeVersion: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val cssFile: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val epubEmbeddedFonts: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @Input @BeanProperty val dataGeneratorClass: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val processMathJaxEscapes: Property[Boolean] =
    getProject.getObjects.property(classOf[Boolean])

  @TaskAction
  def processDocBook(): Unit = {
    val documentName: Option[String] = getDocumentName(document.get)
    val documentNames: List[String] = documents.get.asScala.toList.flatMap(getDocumentName)

    if (documentName.isEmpty && documentNames.isEmpty) throw new IllegalArgumentException(
      """At least one document name must be specified using
        |  document = "<document name>"
        |or
        |  documents = ["<document name>"]
        |""".stripMargin)

    val inputDocuments: List[(String, Boolean)] =
      documentName.toList.map(name => name -> false) ++
      documentNames.map(name => name -> true)

    val processors: List[DocBook2] =
      Option(getProject.findProperty("docBook.outputFormats"))
        .map(_.toString.split(",").map(_.trim).toList.filter(_.nonEmpty))
        .getOrElse(outputFormats.get.asScala.toList)
        .map(DocBook2.forName)

    info(s"Output formats: ${DocBook2.getNames(processors)}")

    val sections: Sections = Sections(mapValues(parameters.get.asScala.toMap)(_.asScala.toMap))

    val unusedSections: Set[Section] =
      sections.usedSections -- processors.flatMap(_.parameterSections).toSet

    if (unusedSections.nonEmpty)
      info(s"Unused parameter sections: ${unusedSections.map(_.name).mkString(", ")}")

    require(!isMathJaxEnabled.get || !isJEuclidEnabled.get)

    val mathJaxConfiguration: mathjax.Configuration = getMathJaxConfiguration

    val substitutionsMap: Map[String, String] = substitutions.get.asScala.toMap

    // FOP configuration
    ProcessDocBookTask.write(
      file = layout.fopConfigurationFile,
      replace = false,
      elem = Fop.defaultConfigurationFile
    )

    // Substitutions DTD
    Files.write(
      file = layout.xmlFile(layout.substitutionsDtdFileName),
      replace = true,
      content = ProcessDocBookTask.substitutionsDtd(substitutionsMap)
    )

    // XML catalog
    ProcessDocBookTask.write(
      file = layout.catalogFile,
      replace = true,
      doctype = Some(Namespace.Catalog.doctype),
      elem = ProcessDocBookTask.xmlCatalog(
        xslt1 = Stylesheets.xslt1.unpack(xslt1version.get, getProject, layout.docBookXslDirectory),
        xslt2 = Stylesheets.xslt2.unpack(xslt2version.get, getProject, layout.docBookXslDirectory),
        catalogGroupBase = layout.catalogGroupBase,
        substitutionsDtdFileName = layout.substitutionsDtdFileName,
        catalogCustomFileName = layout.catalogCustomFileName,
        data = layout.dataDirectoryRelative
      )
    )

    // Custom XML catalog
    ProcessDocBookTask.write(
      file = layout.xmlFile(layout.catalogCustomFileName),
      replace = false,
      doctype = Some(Namespace.Catalog.doctype),
      elem = ProcessDocBookTask.catalogCustomization
    )

    val cssFileName: String = Files.dropAllowedExtension(cssFile.get, "css")

    // CSS file
    Files.write(
      file = layout.cssFile(cssFileName),
      replace = false,
      content = ProcessDocBookTask.defaultCssFile
    )

    // Input documents
    for ((name: String, _ /*prefixed*/: Boolean) <- inputDocuments) ProcessDocBookTask.write(
      file = layout.inputFile(name),
      replace = false,
      doctype = Some(Namespace.DocBook.doctype),
      elem = ProcessDocBookTask.defaultInputFile
    )

    val fontFamilyNames: List[String] = epubEmbeddedFonts.get.asScala.toList
    val epubEmbeddedFontsUris: List[URI] = FopFonts.getFiles(layout.fopConfigurationFile, fontFamilyNames)
    val epubEmbeddedFontsString: String = epubEmbeddedFontsUris.map(uri => new File(uri.getPath).getAbsolutePath).mkString(", ")
    info(s"Fop.getFontFiles(${fontFamilyNames.mkString(", ")}) = $epubEmbeddedFontsString.")

    val enableMathJax: Boolean = isMathJaxEnabled.get || isJEuclidEnabled.get

    // Custom stylesheet
    for (section: CommonSection <- CommonSection.all) ProcessDocBookTask.write(
      file = layout.stylesheetFile(layout.customStylesheet(section)),
      replace = false,
      elem = section.customStylesheet
    )
    for (variant: Variant <- sections.allVariants) ProcessDocBookTask.write(
      file = layout.stylesheetFile(layout.customStylesheet(variant)),
      replace = false,
      elem = variant.docBook2.customStylesheet
    )

    // Parameters stylesheet
    for (variant: Variant <- sections.allVariants) ProcessDocBookTask.write(
      file = layout.stylesheetFile(layout.paramsStylesheet(variant)),
      replace = true,
      elem = variant.docBook2.paramsStylesheet(sections.parameters(variant))
    )

    // Main stylesheet
    for {
      variant: Variant <- sections.allVariants
      (documentName: String, prefixed: Boolean) <- inputDocuments
    } {
      val forDocument: Layout.ForDocument = layout.forDocument(prefixed, documentName)

      // xsl:param has the last value assigned to it, so customization must come last;
      // since it is imported (so as not to be overwritten), and import elements must come first,
      // a separate "-param" file is written with the "default" values for the parameters :)
      ProcessDocBookTask.write(
        file = layout.stylesheetFile(forDocument.mainStylesheet(variant)),
        replace = true,
        elem = variant.docBook2.mainStylesheet(
          paramsStylesheetName = layout.paramsStylesheet(variant),
          stylesheetUriBase = (if (variant.docBook2.usesDocBookXslt2) Stylesheets.xslt2 else Stylesheets.xslt1).uri,
          customStylesheets =
            variant.docBook2.commonSections.map(layout.customStylesheet) ++
            (variant.baseVariant.toSeq :+ variant).map(layout.customStylesheet),
          values = new NonOverridableParameters(
            isInfoEnabled = logger.isInfoEnabled,
            embeddedFonts = epubEmbeddedFontsString,
            cssFile = layout.cssFileRelativeToOutputDirectory(cssFileName),
            imagesDirectoryName = layout.imagesDirectoryName,
            mathJaxConfiguration = if(!enableMathJax) None else Some(mathJaxConfiguration),
            documentName = documentName,
            saxonOutputDirectory = forDocument.saxonOutputDirectory(variant)
          )
        )
      )
    }

    // Data generation
    val mainClass: String = dataGeneratorClass.get
    if (mainClass.isEmpty) info("Skipping DocBook data generation: dataGenerationClass is not set")
    else generateData(mainClass)

    // MathJax
    val mathJax: Option[MathJax] = if (!processors.exists(_.isPdf) || !isMathJaxEnabled.get) None
    else Some(MathJax.get(
      getProject,
      nodeRoot = layout.nodeRoot,
      nodeVersion = nodeVersion.get,
      overwriteNode = false,
      overwriteMathJax = false,
      j2v8Parent = if (!useJ2V8.get) None else Some(layout.j2v8LibraryDirectory),
      configuration = mathJaxConfiguration))


    val variants: Seq[Variant] = sections.runVariants(processors)
    logger.lifecycle("Output variants: " + variants.map(_.fullName).mkString("[", ", ", "]"))

    val processDocBook: ProcessDocBook = new ProcessDocBook(
      project = getProject,
      // In processing instructions and CSS, substitute xslParameters also - because why not?
      substitutions = sections.substitutions ++ substitutionsMap,
      resolver = new Resolver(layout.catalogFile),
      isJEuclidEnabled = isJEuclidEnabled.get,
      mathJax,
      fopConfigurationFile = layout.fopConfigurationFile,
      imagesDirectory = layout.imagesDirectory,
      imagesDirectoryName = layout.imagesDirectoryName,
      cssDirectory = layout.cssDirectory,
      cssDirectoryName = layout.cssDirectoryName
    )

    // Process DocBook :)
    for {
      variant: Variant <- variants
      (documentName: String, prefixed: Boolean) <- inputDocuments
    } {
      logger.lifecycle(s"DocBook: processing '$documentName' to ${variant.fullName}.")

      val forDocument: Layout.ForDocument = layout.forDocument(prefixed, documentName)

      processDocBook.run(
        docBook2 = variant.docBook2,
        inputFile = layout.inputFile(documentName),
        stylesheetFile = layout.stylesheetFile(forDocument.mainStylesheet(variant)),
        saxonOutputDirectory = forDocument.saxonOutputDirectory(variant),
        saxonOutputFile = forDocument.saxonOutputFile(variant),
        outputDirectory = forDocument.outputDirectory(variant),
        outputFile = forDocument.outputFile(variant)
      )
    }
  }

  private def getDocumentName(string: String): Option[String] =
    if (string.isEmpty) None else Some(Files.dropAllowedExtension(string, "xml"))

  private def generateData(mainClass: String): Unit = {
    val mainSourceSet: Option[SourceSet] = Gradle.mainSourceSet(getProject)
    if (mainSourceSet.isEmpty) logger.warn(
      s"Skipping DocBook data generation: no Java plugin in the project") else
    {
      val dataDirectory: File = layout.dataDirectory
      info(s"Running DocBook data generator $mainClass into $dataDirectory")
      Gradle.javaexec(
        getProject,
        mainClass,
        mainSourceSet.get.getRuntimeClasspath,
        dataDirectory.toString
       )
    }
  }

  private def getMathJaxConfiguration: mathjax.Configuration = {
    def delimiters(property: Property[String]): Seq[mathjax.Configuration.Delimiters] =
      Seq(new mathjax.Configuration.Delimiters(property.get, property.get))

    mathjax.Configuration(
      font = mathJaxFont.get,
      extensions = mathJaxExtensions.get.asScala.toList,
      texDelimiters = delimiters(texDelimiter),
      texInlineDelimiters = delimiters(texInlineDelimiter),
      asciiMathDelimiters = delimiters(asciiMathDelimiter),
      processEscapes = processMathJaxEscapes.get
    )
  }
}

private object ProcessDocBookTask {

  def write(
    file: File,
    replace: Boolean,
    doctype: Option[String] = None,
    elem: Elem
  ): Unit = Files.write(
    file,
    replace,
    content = PrettyPrinter.default.renderXml(doctype = doctype, element = elem)
  )

  def xmlCatalog(
    xslt1: File,
    xslt2: File,
    catalogGroupBase: String,
    substitutionsDtdFileName: String,
    catalogCustomFileName: String,
    data: String
  ): Elem =
    <catalog xmlns={Namespace.Catalog.uri} prefer="public">
      {Comment(s" DO NOT EDIT! Generated by the DocBook plugin. Customizations go into $catalogCustomFileName. ")}
      <group xml:base={catalogGroupBase}>
        <!--
          There seems to be some confusion with the rewriteURI form:
          Catalog DTD requires 'uriIdStartString' attribute (and that is what IntelliJ wants),
          but XMLResolver looks for the 'uriStartString' attribute (and this seems to work in Oxygen).
        -->

        <!-- DocBook XSLT 1.0 stylesheets  -->
        <rewriteURI uriStartString="http://docbook.sourceforge.net/release/xsl-ns/current/"
                    rewritePrefix={s"$xslt1/"}/>

        <!-- DocBook XSLT 2.0 stylesheets  -->
        <rewriteURI uriStartString="https://cdn.docbook.org/release/latest/xslt/"
                    rewritePrefix={s"$xslt2/"}/>

        <!-- generated data -->
        <rewriteSystem systemIdStartString="data:/"
                       rewritePrefix={data}/>
        <rewriteSystem systemIdStartString="data:"
                       rewritePrefix={data}/>
        <rewriteSystem systemIdStartString="urn:docbook:data:/"
                       rewritePrefix={data}/>
        <rewriteSystem systemIdStartString="urn:docbook:data:"
                       rewritePrefix={data}/>
        <rewriteSystem systemIdStartString="urn:docbook:data/"
                       rewritePrefix={data}/>
        <rewriteSystem systemIdStartString="http://opentorah.org/docbook/data/"
                       rewritePrefix={data}/>
      </group>

      <!-- substitutions DTD -->
      <public publicId={Namespace.DocBook.dtdId}
              uri={substitutionsDtdFileName}/>

      <nextCatalog catalog={catalogCustomFileName}/>
    </catalog>

  val catalogCustomization: Elem =
    <catalog xmlns={Namespace.Catalog.uri} prefer="public">
      <!-- Customizations go here. -->
      <nextCatalog catalog="/etc/xml/catalog"/>
    </catalog>

  def substitutionsDtd(substitutions: Map[String, String]): String = substitutions.toSeq.map {
    case (name: String, value: String) => s"""<!ENTITY $name "$value">\n"""
  }.mkString

  val defaultInputFile: Elem =
    <article xmlns={Namespace.DocBook.uri} version={Namespace.DocBook.version} xmlns:xi={Namespace.XInclude.uri}/>

  val defaultCssFile: String =
    s"""@namespace xml "${Namespace.Xml.uri}";
        |""".stripMargin
}
