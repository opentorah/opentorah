package org.podval.docbook.gradle.plugin

import java.io.File

import org.gradle.api.Project
import org.podval.docbook.gradle.mathjax
import org.podval.docbook.gradle.section.DocBook2
import org.podval.docbook.gradle.util.{Gradle, Logger, Util}
import org.podval.docbook.gradle.xml.{ProcessingInstructionsFilter, Resolver, Xml}
import org.xml.sax.XMLReader

final class ProcessDocBook(
  project: Project,
  substitutions: Map[String, String],
  resolver: Resolver,
  isJEuclidEnabled: Boolean,
  mathJaxTypesetter: Option[mathjax.Typesetter],
  layout: Layout,
  logger: Logger
) {
  def run(
    docBook2: DocBook2,
    prefixed: Boolean,
    documentName: String
  ): Unit = {
    logger.lifecycle(s"DocBook: processing '$documentName' to ${docBook2.name}.")

    val forDocument: Layout.ForDocument = layout.forDocument(prefixed, documentName)

    val saxonOutputDirectory: File = forDocument.saxonOutputDirectory(docBook2)
    saxonOutputDirectory.mkdirs

    val saxonOutputFile: File = forDocument.saxonOutputFile(docBook2)

    // do not output the 'main' file when chunking in XSLT 1.0
    val outputFile: Option[File] = if (docBook2.usesRootFile) Some(saxonOutputFile) else None

    val xmlReader: XMLReader = Xml.getFilteredXMLReader(
      Seq(new ProcessingInstructionsFilter(substitutions, resolver, logger)) ++
        mathJaxTypesetter.flatMap(mathJaxTypesetter => docBook2.xmlFilter(mathJaxTypesetter.configuration)).toSeq
      // ++ Seq(new TracingFilter)
    )

    // Run Saxon.
    Xml.transform(
      useSaxon9 = docBook2.usesDocBookXslt2,
      resolver,
      inputFile = layout.inputFile(documentName),
      stylesheetFile = layout.stylesheetFile(forDocument.mainStylesheet(docBook2)),
      xmlReader,
      outputFile,
      logger
    )

    copyImagesAndCss(docBook2, saxonOutputDirectory)

    // Post-processing.
    if (docBook2.usesIntermediate) {
      logger.info(s"Post-processing ${docBook2.name}")
      val outputDirectory: File = forDocument.outputDirectory(docBook2)
      outputDirectory.mkdirs

      docBook2.postProcess(
        fopConfigurationFile = layout.fopConfigurationFile,
        substitutions,
        isJEuclidEnabled,
        mathJaxTypesetter,
        inputDirectory = saxonOutputDirectory,
        inputFile = saxonOutputFile,
        outputFile = forDocument.outputFile(docBook2),
        logger
      )
    }
  }

  private def copyImagesAndCss(
    docBook2: DocBook2,
    saxonOutputDirectory: File
  ): Unit = {
    val into: File = Util.prefixedDirectory(saxonOutputDirectory, docBook2.copyDestinationDirectoryName)

    logger.info(s"Copying images")
    Gradle.copyDirectory(project,
      into,
      from = layout.imagesDirectory.getParentFile,
      directoryName = layout.imagesDirectoryName
    )

    if (docBook2.usesCss) {
      logger.info(s"Copying CSS")
      Gradle.copyDirectory(project,
        into,
        from = layout.cssDirectory.getParentFile,
        directoryName = layout.cssDirectoryName,
        substitutions = substitutions
      )
    }
  }
}
