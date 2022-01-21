package org.opentorah.docbook.plugin

import org.gradle.api.{Action, DefaultTask, NamedDomainObjectContainer, Plugin, Project, Task}
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.gradle.api.tasks.{Internal, Nested, TaskAction}
import org.opentorah.docbook.{CommonConfiguration, DocBookConfiguration, DocBookProcessor, DocumentConfiguration,
  FormatConfiguration, Layout, VariantConfiguration}
import org.opentorah.fop.FopFonts
import org.opentorah.html.SiteHtml
import org.opentorah.math.{Delimiters, MathConfiguration}
import org.opentorah.util.{BuildContext, GradleBuildContext}
import org.opentorah.xml.{From, Parser}
import java.io.File
import javax.inject.Inject
import scala.jdk.CollectionConverters.*

// Note: https://docs.gradle.org/current/userguide/custom_gradle_types.html was a kind of an eye-opener :)
// Additional lessons learned:
// - do NOT make Task and DSL classes final so that Gradle could decorate them;
// - do NOT name a property 'extensions' - it has special meaning for Gradle;
// - declare nested DSL as @Nested def getXXX(): T, NOT as def getXXX(): Property[T];
// - for the {} syntax to work in addition to the dotted one,
//   def XXX(action: Action[T]): Unit = action.execute(getXXX()) also.
//   (see https://discuss.gradle.org/t/multi-level-dsl-for-plugin-extension/19029/11
//    and https://dev.to/autonomousapps/gradle-plugins-and-extensions-a-primer-for-the-bemused-51lp)

final class DocBookPlugin extends Plugin[Project]:

  override def apply(project: Project): Unit =
    //context.info(Platform.applicationString)

    project.getExtensions.create("docBook", classOf[DocBookPlugin.Extension])

    project.getTasks.create("installDocBookDependencies", classOf[DocBookPlugin.InstallDocBookDependenciesTask])
    project.getTasks.create("processDocBook"            , classOf[DocBookPlugin.ProcessDocBookTask            ])
    project.getTasks.create("listFopFonts"              , classOf[DocBookPlugin.ListFopFontsTask              ])
    project.getTasks.create("deleteFopFontsCache"       , classOf[DocBookPlugin.DeleteFopFontsCacheTask       ])

object DocBookPlugin:

  trait ToConfiguration[T]:
    def getName: String // Type must have a read-only 'name' property
    def getParameters: MapProperty[String, String]
    def parameters: Map[String, String] = toMap(getParameters)
    def toConfiguration: T

  trait SettingsProperties:
    def getEpubEmbeddedFonts: ListProperty[String]
    def epubEmbeddedFonts: List[String] = toList(getEpubEmbeddedFonts)
    def getXslt1version: Property[String]
    def xslt1version: Option[String] = optional(getXslt1version)
    def getXslt2version: Property[String]
    def xslt2version: Option[String] = optional(getXslt2version)
    @Nested def getMath: MathConfigurationProperties
    def math(action: Action[MathConfigurationProperties]): Unit = action.execute(getMath)
    def math: Option[MathConfiguration] = Some(getMath.toConfiguration)

  trait OutputProperties extends SettingsProperties:
    def getSubstitutions: MapProperty[String, String]
    def substitutions: Map[String, String] = toMap(getSubstitutions)
    def getOutput: ListProperty[String]
    def output: Set[String] = toList(getOutput).toSet

  abstract class Extension @Inject(project: Project) extends OutputProperties:

    // Verify the configuration when it has been evaluated
    project.afterEvaluate((_: Project) => getProcessor.verify())

    def getGlobalSubstitutions: MapProperty[String, String]
    def getConfiguration: Property[String]
    def getHtmlConfiguration: Property[String]
    def getDocuments: NamedDomainObjectContainer[DocumentProperties]
    def getCommon: NamedDomainObjectContainer[CommonProperties]
    def getFormats: NamedDomainObjectContainer[FormatProperties]

    private def toConfiguration: DocBookConfiguration = DocBookConfiguration(
      output = output,
      math = math,
      substitutions = substitutions,
      xslt1version = xslt1version,
      xslt2version = xslt2version,
      epubEmbeddedFonts = epubEmbeddedFonts,
      documents = toSet(getDocuments),
      common = toSet(getCommon),
      formats = toSet(getFormats)
    )

    private var processor: Option[DocBookProcessor] = None
    def getProcessor: DocBookProcessor =
      if processor.isEmpty then
        val layout: Layout = Layout.forGradleProject(project)
        val context: BuildContext = BuildContext.forGradleProject(project)
        val configurationFromExtension: DocBookConfiguration = toConfiguration
        val configurationFile: File = File(layout.root, optional(getConfiguration).getOrElse("docbook.xml"))
        val configuration: DocBookConfiguration = if !configurationFile.exists() then configurationFromExtension else
          if !configurationFromExtension.isEmpty
          then context.warn(s"DocBook: configuration file will be used and configuration in the extension ignored.")

          context.info(s"DocBook: reading configuration from file: $configurationFile")
          Parser.unsafeRun(DocBookConfiguration.parse(From.file(configurationFile)))

        val htmlConfigurationFile: File = File(layout.root, optional(getHtmlConfiguration).getOrElse("html.xml"))
        val siteHtml: SiteHtml =
          if !htmlConfigurationFile.exists()
          then SiteHtml.empty
          else Parser.unsafeRun(SiteHtml.parse(From.file(htmlConfigurationFile)))

        processor = Some(configuration.toProcessor(
          layout = layout,
          context = context,
          siteHtml = siteHtml
        ))
      processor.get

  abstract class DocumentProperties extends ToConfiguration[DocumentConfiguration], OutputProperties:
    def getDataGeneratorClass: Property[String]
    def getImagesDirectory: Property[String]

    override def toConfiguration: DocumentConfiguration = DocumentConfiguration(
      name = getName,
      output = output,
      parameters = parameters,
      math = math,
      substitutions = substitutions,
      xslt1version = xslt1version,
      xslt2version = xslt2version,
      epubEmbeddedFonts = epubEmbeddedFonts,
      dataGeneratorClass = optional(getDataGeneratorClass),
      imagesDirectory = optional(getImagesDirectory)
    )

  abstract class CommonProperties extends ToConfiguration[CommonConfiguration]:
    override def toConfiguration: CommonConfiguration = CommonConfiguration(
      name = getName,
      parameters = parameters
    )

  abstract class FormatProperties extends ToConfiguration[FormatConfiguration], SettingsProperties:
    def getVariants: NamedDomainObjectContainer[VariantConfigurationProperties]

    override def toConfiguration: FormatConfiguration = FormatConfiguration(
      name = getName,
      parameters = parameters,
      math = math,
      xslt1version = xslt1version,
      xslt2version = xslt2version,
      epubEmbeddedFonts = epubEmbeddedFonts,
      variants = toSet(getVariants)
    )

  abstract class VariantConfigurationProperties extends ToConfiguration[VariantConfiguration], SettingsProperties:
    override def toConfiguration: VariantConfiguration = VariantConfiguration(
      name = getName,
      parameters = parameters,
      math = math,
      xslt1version = xslt1version,
      xslt2version = xslt2version,
      epubEmbeddedFonts = epubEmbeddedFonts
    )

  abstract class MathConfigurationProperties:
    def getJEuclidEnabled     : Property[Boolean]
    def getMathJaxEnabled     : Property[Boolean]
    def getNodeVersion        : Property[String]
    def getUseMathJaxV3       : Property[Boolean]
    def getUseJ2V8            : Property[Boolean]
    def getFont               : Property[String]
    def getMathJaxExtensions  : ListProperty[String]
    def getTexExtensions      : ListProperty[String]
    def getProcessEscapes     : Property[Boolean]
    def getTexDelimiters      : ListProperty[String]
    def getTexInlineDelimiters: ListProperty[String]
    def getAsciiMathDelimiters: ListProperty[String]

    def toConfiguration: MathConfiguration = MathConfiguration(
      jEuclidEnabled      = optional(getJEuclidEnabled),
      mathJaxEnabled      = optional(getMathJaxEnabled),
      nodeVersion         = optional(getNodeVersion),
      useMathJaxV3        = optional(getUseMathJaxV3),
      useJ2V8             = optional(getUseJ2V8),
      font                = optional(getFont),
      mathJaxExtensions   = toList(getMathJaxExtensions  ),
      texExtensions       = toList(getTexExtensions      ),
      texDelimiters       = toList(getTexDelimiters      ).map(Delimiters.fromString), // TODO split start/end
      texInlineDelimiters = toList(getTexInlineDelimiters).map(Delimiters.fromString), // TODO split start/end
      asciiMathDelimiters = toList(getAsciiMathDelimiters).map(Delimiters.fromString), // TODO split start/end
      processEscapes      = optional(getProcessEscapes)
    )

//  abstract class DelimitersProperties:
//    def getStart: Property[String]
//    def getEnd  : Property[String]
//
//    def toDelimiters: Delimiters = Delimiters(
//      start = getStart.get(),
//      end   = getEnd  .get()
//    )

  private def optional[T](property: Property[T]): Option[T] =
    if !property.isPresent then None else Some(property.get)

  private def toList[T](property: ListProperty[T]): List[T] =
    property.get().asScala.toList

  private def toMap(property: MapProperty[String, String]): Map[String, String] =
    property.get().asScala.toMap

  private def toSet[C, T <: ToConfiguration[C]](container: NamedDomainObjectContainer[T]): Set[C] =
    container.asScala.toSet.map(_.toConfiguration)

  class ListFopFontsTask extends DefaultTask:
    setDescription("List FOP fonts")
    @TaskAction def execute(): Unit =
      val result: String = FopFonts.list(Layout.forGradleProject(getProject).fopConfigurationFile)
      System.out.print(result)
      System.out.flush()

  class DeleteFopFontsCacheTask extends DefaultTask:
    setDescription("Delete FOP fonts cache")
    @TaskAction def execute(): Unit =
      FopFonts.deleteCache(Layout.forGradleProject(getProject).fopConfigurationFile)

  class InstallDocBookDependenciesTask extends DefaultTask:
    setDescription(s"Install DocBook dependencies")
    setGroup("publishing")
    @TaskAction def execute(): Unit =
      val extension: Extension = getProject.getExtensions.getByType(classOf[Extension])
      extension.getProcessor.installDistibutions

  class ProcessDocBookTask extends DefaultTask:
    setDescription(s"Process DocBook")
    setGroup("publishing")
    // Register inputs and outputs
    // Note: deleting output directories (with Files.deleteRecursively(directory)) makes the task not up-to-date.
    // TODO - really?!
    private val layout: Layout = Layout.forGradleProject(getProject)
    Set(layout.src            ).foreach(getInputs .dir)
    Set(layout.tmp, layout.out).foreach(getOutputs.dir)
    // To let projects that use the plugin to not make assumptions about directory names:
    @Internal def getOutputDirectory: File = layout.out
    // Note: even when DocBook plugin is applied after the Scala one,
    // there is no 'classes' task during its application - but there is after project evaluation:
    getProject.afterEvaluate((project: Project) =>
      val context: BuildContext = BuildContext.forGradleProject(project)
      val classesTask: Option[Task] = GradleBuildContext.getClassesTask(project)
      if classesTask.isEmpty then
        context.info("No 'classes' task found.")
      else
        context.info("Found 'classes' task; adding it as dependency of 'processDocBook'.")
        getDependsOn.add(classesTask.get)

      () // return Unit to help the compiler find the correct overload
    )
    @TaskAction def processDocBook(): Unit =
      val extension: Extension = getProject.getExtensions.getByType(classOf[Extension])
      extension.getProcessor.process(
        globalSubstitutions = toMap(extension.getGlobalSubstitutions)
      )
