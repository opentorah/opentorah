package org.podval.docbook.gradle

import org.gradle.api.DefaultTask
import org.gradle.api.file.CopySpec
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.gradle.api.tasks.{Input, Internal, TaskAction}
import java.io.{File, FileWriter}
import javax.xml.transform.stream.{StreamResult, StreamSource}
import org.apache.tools.ant.filters.ReplaceTokens
import org.xml.sax.InputSource
import scala.beans.BeanProperty
import scala.collection.JavaConverters._
import org.podval.docbook.gradle.section.{DocBook2, Section}

class ProcessDocBookTask extends DefaultTask {

  private val layout: Layout = Layout.forProject(getProject)

  private val logger: Logger = new Logger.PluginLogger(getLogger)

  // To let projects that use the plugin to not make assumptions about directory names:
  @Internal def getOutputDirectory: File = layout.outputRoot

  // Register inputs
  val inputDirectories: Set[File] = Set(
    layout.inputDirectory,
    layout.cssDirectory,
    layout.fopConfigurationDirectory,
    layout.dataDirectory,
    layout.stylesheetDirectory
  ) ++ Set(
    layout.imagesDirectory,
  ).filter(_.exists)

  inputDirectories.foreach { directory: File =>
    logger.info(s"Registering input directory $directory")
    directory.mkdirs()
    getOutputs.dir(directory)
  }

  // Register outputs
  Set(
    layout.intermediateRoot,
    layout.outputRoot
  ).foreach { directory: File =>
    Util.deleteRecursively(directory)
    logger.info(s"Registering output directory $directory")
    getOutputs.dir(directory)
  }

  @Input @BeanProperty val document: Property[String] =
    getProject.getObjects.property(classOf[String])

  @BeanProperty val documents: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @BeanProperty val parameters: MapProperty[String, java.util.Map[String, String]] =
    getProject.getObjects.mapProperty(classOf[String], classOf[java.util.Map[String, String]])

  @Input @BeanProperty val substitutions: MapProperty[String, String] =
    getProject.getObjects.mapProperty(classOf[String], classOf[String])

  @Input @BeanProperty val outputFormats: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @Input @BeanProperty val isMathJaxEnabled: Property[Boolean] =
    getProject.getObjects.property(classOf[Boolean])

  @Input @BeanProperty val isJEuclidEnabled: Property[Boolean] =
    getProject.getObjects.property(classOf[Boolean])

  @TaskAction
  def processDocBook(): Unit = {
    val (documentName: Option[String], documentNames: List[String]) =
      Util.documentNames(document, documents)

    val processors: List[DocBook2] =
      Option(getProject.findProperty("docBook.outputFormats"))
        .map(_.toString.split(",").map(_.trim).toList.filter(_.nonEmpty))
        .getOrElse(outputFormats.get.asScala.toList)
        .map(DocBook2.forName)

    logger.info(s"Output formats: ${DocBook2.getNames(processors)}")

    val sections: Map[Section, Map[String, String]] = Util.getSections(parameters)

    val unusedSections: Set[Section] =
      sections.keySet -- processors.flatMap(_.parameterSections).toSet
    if (unusedSections.nonEmpty)
      logger.info(s"Unused parameter sections: ${unusedSections.map(_.name).mkString(", ")}")

    // In processing instructions and CSS, substitute xslParameters also - because why not?
    val allSubstitutions: Map[String, String] =
      sections.values.toList.flatten.toMap ++ substitutions.get.asScala.toMap

    val resolver: Resolver = new Resolver(layout.catalogFile,  logger)

    for (docBook2: DocBook2 <- processors) {
      documentName.foreach { documentName: String =>
        run(
          docBook2 = docBook2,
          prefixed = false,
          documentName = documentName,
          substitutions = allSubstitutions,
          resolver = resolver
        )
      }

      documentNames.foreach { documentName: String =>
        run(
          docBook2 = docBook2,
          prefixed = true,
          documentName = documentName,
          substitutions = allSubstitutions,
          resolver = resolver
        )
      }
    }
  }

  final def run(
    docBook2: DocBook2,
    prefixed: Boolean,
    documentName: String,
    substitutions: Map[String, String],
    resolver: Resolver
  ): Unit = {
    logger.lifecycle(s"DocBook: processing '$documentName' to ${docBook2.name}.")

    val forDocument = layout.forDocument(prefixed, documentName)

    // Saxon output directory.
    val saxonOutputDirectory: File = forDocument.saxonOutputDirectory(docBook2)
    saxonOutputDirectory.mkdirs

    // Saxon output file and target.
    val saxonOutputFile: File = forDocument.saxonOutputFile(docBook2)
    val outputTarget = new StreamResult
    // null the outputTarget when chunking in XSLT 1.0
    if (docBook2.usesRootFile) {
      outputTarget.setSystemId(saxonOutputFile)
      outputTarget.setWriter(new FileWriter(saxonOutputFile))
    } else {
      outputTarget.setSystemId("dev-null")
      outputTarget.setOutputStream((_: Int) => {})
    }

    // Run Saxon.
    val mainStylesheetName: String = forDocument.mainStylesheet(docBook2)
    Saxon.run(
      inputSource = new InputSource(layout.inputFile(documentName).toURI.toASCIIString),
      stylesheetSource = new StreamSource(layout.stylesheetFile(mainStylesheetName)),
      outputTarget = outputTarget,
      resolver = resolver,
      processingInstructionsSubstitutions = substitutions,
      useXslt2 = docBook2.usesDocBookXslt2,
      logger = logger
    )

    val copyDestinationDirectory: File =
      docBook2.copyDestinationDirectoryName.fold(saxonOutputDirectory)(new File(saxonOutputDirectory, _))

    // Copy images.
    logger.info(s"Copying images")
    getProject.copy((copySpec: CopySpec) => copySpec
      .into(copyDestinationDirectory)
      .from(layout.imagesDirectory.getParentFile)
      .include(layout.imagesDirectoryName + "/**")
    )

    // Copy CSS.
    if (docBook2.usesCss) {
      logger.info(s"Copying CSS")
      getProject.copy((copySpec: CopySpec) => copySpec
        .into(copyDestinationDirectory)
        .from(layout.cssDirectory.getParentFile)
        .include(layout.cssDirectoryName + "/**")
        .filter(Map("tokens" -> substitutions.asJava).asJava, classOf[ReplaceTokens])
      )
    }

    // Post-processing.
    if (docBook2.usesIntermediate) {
      logger.info(s"Post-processing ${docBook2.name}")
      val outputDirectory: File = forDocument.outputDirectory(docBook2)
      outputDirectory.mkdirs

      docBook2.postProcess(
        layout = layout,
        substitutions = substitutions,
        isMathJaxEnabled = isMathJaxEnabled.get,
        isJEuclidEnabled = isJEuclidEnabled.get,
        inputDirectory = saxonOutputDirectory,
        inputFile = saxonOutputFile,
        outputFile = forDocument.outputFile(docBook2),
        logger = logger
      )
    }
  }
}
