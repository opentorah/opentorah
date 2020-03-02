package org.digitaljudaica.archive.collector

import java.io.File
import org.digitaljudaica.archive.collector.reference.{Reference, ReferenceSource}
import org.digitaljudaica.reference.Entity
import org.digitaljudaica.tei.Tei
import org.digitaljudaica.xml.{From, Parser, Print}
import scala.xml.{Elem, Node}

final class Document(
  layout: Layout,
  collection: Collection,
  override val name: String,
  prev: Option[String],
  next: Option[String],
  val translations: Seq[String]
) extends ReferenceSource(collection) {
  override def toString: String = s"$collection:$name"

  private val tei: Tei = Parser.parseDo(Tei.parse(From.file(collection.teiDirectory, name)))

  override val references: Seq[Reference] = bindReferences(tei.references)

  override def isNames: Boolean = false

  override def viewer: String = "documentViewer"

  override def url: String = layout.documentUrl(collection.directoryName, name)

  val title: Option[Seq[Node]] = tei.titleStmt.titles.headOption.map(_.content)

  def authors: Seq[Seq[Node]] = tei.titleStmt.authors.map(_.xml)

  def transcribers: Seq[Elem] = tei.titleStmt.editors.filter(_.role.contains("transcriber")).flatMap(_.persName)

  def date: Option[String] = tei.teiHeader.profileDesc.flatMap(_.creation.map(_.date.when))

  def description: Option[Seq[Node]] = tei.getAbstract.orElse(title)

  def language: Option[String] = tei.teiHeader.profileDesc.flatMap(_.langUsage).toSeq.flatMap(_.languages).map(_.ident).headOption

  val pages: Seq[Page] = for (pb <- tei.pbs) yield collection.pageType(
    n = pb.n,
    facs = pb.facs
  )

  def addressee: Option[Reference] =
    references.find(name => (name.entity == Entity.Person) && name.role.contains("addressee"))

  def writeWrappers(docsDirectory: File, facsDirectory: File): Unit = {
    import Util.quote
    val navigation: Seq[(String, String)] =
      Seq("documentCollection" -> quote(collection.reference)) ++
      prev.map(prev => Seq("prevDocument" -> quote(prev))).getOrElse(Seq.empty) ++
      Seq("thisDocument" -> quote(name)) ++
      next.map(next => Seq("nextDocument" -> quote(next))).getOrElse(Seq.empty)

    def writeTeiWrapper(name: String, lang: Option[String]): Unit = {
      val nameWithLang: String = lang.fold(name)(lang => name + "-" + lang)

      Util.writeTeiWrapper(
        directory = docsDirectory,
        fileName = nameWithLang,
        teiPrefix = Some(s"../${layout.teiDirectoryName}/"),
        target = "documentViewer",
        yaml = Seq(
          "facs" -> s"'../${layout.facsDirectoryName}/$name.html'"
        ) ++ (if (lang.isDefined || translations.isEmpty) Seq.empty else Seq("translations" -> translations.mkString("[", ", ", "]")))
          ++ navigation
      )
    }

    // TEI wrapper(s)
    writeTeiWrapper(name, None)
    for (lang <- translations) writeTeiWrapper(name, Some(lang))

    // Facsimile viewer
    val facsimilePages: Elem =
      <div class="facsimileViewer">
        <div class="facsimileScroller">{
          for (page: Page <- pages.filter(_.isPresent); n = page.n) yield {
            <a target="documentViewer" href={s"../${layout.documentsDirectoryName}/$name.html#p$n"}>
              <figure>
                <img xml:id={s"p$n"} alt={s"facsimile for page $n"} src={page.facs.orNull}/>
                <figcaption>{n}</figcaption>
              </figure>
            </a>}}
        </div>
      </div>

    Util.writeWithYaml(
      file = Util.htmlFile(facsDirectory, name),
      layout = "default",
      yaml = Seq(
        "transcript" -> s"'../${layout.documentsDirectoryName}/$name.html'"
      )
        ++ navigation,
      content = Seq(Print.render(facsimilePages) + "\n")
    )
  }
}
