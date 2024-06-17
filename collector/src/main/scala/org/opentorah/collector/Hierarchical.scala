package org.opentorah.collector

import org.opentorah.html.A
import org.opentorah.metadata.{Language, Names}
import org.opentorah.tei.{Abstract, Body, Tei, Title}
import org.opentorah.store.{Context, Path, Pure, Store}
import org.opentorah.xml.{Element, ElementTo, Elements, ElementsTo, FromUrl, Nodes, Parsable, Parser}
import zio.ZIO

// TODO push Hierarchical/ByHierarchy into Site;
// TODO push up Collector-specific stuff (if any);
// TODO do 'texts' site!
abstract class Hierarchical(
  override val fromUrl: FromUrl,
  override val names: Names,
  val title: Title.Value,
  val description: Option[Abstract.Value],
  val body: Option[Body.Value],
) extends
  FromUrl.With,
  Pure[Store]:

  final def titleString: String = title.content.toString

  final override def htmlHeadTitle: Option[String] = Some(titleString)
  final override def htmlBodyTitle: Option[Nodes] = None

  final def displayTitle: String = Hierarchical.displayName(this) + ": " + titleString

  final def descriptionNodes: Nodes = description.toSeq.map(Abstract.element.xmlElement)

  final def reference(context: Context, path: Path): Parser[Element] =
    for a: A <- context.a(path) yield a(text = displayTitle)
    
  final def pathHeaderHorizontal(path: Path): String =
    @scala.annotation.tailrec
    def pathHeaderHorizontal(path: Path, result: Seq[String]): Seq[String] =
      if path.isEmpty then result else pathHeaderHorizontal(
        path = path.tail.tail,
        result = result :+ s"${Hierarchical.displayName(path.head)} ${Hierarchical.displayName(path.tail.head)}"
      )

    pathHeaderHorizontal(path, Seq.empty).mkString(", ")

  final def pathHeaderVertical(context: Context, path: Path): Parser[Elements] =
    def pathHeaderVertical(
      path: Path,
      pathTail: Path,
      result: Elements
    ): Parser[Elements] = if pathTail.isEmpty then ZIO.succeed(result) else
      val pathNew: Path = path ++ pathTail.take(2)
      val hierarchical: Hierarchical = pathTail.tail.head.asInstanceOf[Hierarchical]
      for
        a: A <- context.a(pathNew)
        // TODO drop the colon if the title is empty
        line: Element =
          <l>
            {Hierarchical.displayName(pathTail.head)}
            {a(text = Hierarchical.displayName(hierarchical))}:
            {hierarchical.title.content}
          </l>
        result: Elements <- pathHeaderVertical(
          path = pathNew,
          pathTail = pathTail.drop(2),
          result = result :+ line
        )
      yield
        result

    pathHeaderVertical(path = Seq.empty, pathTail = path, result = Seq.empty)

  final override def header(path: Path, context: Context): Parser[Option[Element]] = for
    pathHeader: Elements <- pathHeaderVertical(context, path.dropRight(2))
  yield Some(
    <div class="store-header">
      {pathHeader}
      <head xmlns={Tei.namespace.uri}>
        {Hierarchical.displayName(path.init.last)}
        {Hierarchical.displayName(this)}:
        {title.content}
      </head>
      {descriptionNodes}
      {body.toSeq.map(_.content)}
    </div>
  )

  def getBy: Option[ByHierarchy]

object Hierarchical extends ElementsTo.Union[Hierarchical]:
  protected def elements: Seq[ElementTo[? <: Hierarchical]] = Seq(Hierarchy, Collection)

  override protected def elementByValue(value: Hierarchical): ElementTo[? <: Hierarchical] = value match
    case _: Hierarchy  => Hierarchy
    case _: Collection => Collection

  val namesParsable: Parsable[Names] = Names.withDefaultNameParsable
  val titleElement: ElementsTo.Required[Title.Value] = Title.element.required
  val descriptionElement: ElementsTo.Optional[Abstract.Value] = Abstract.element.optional
  val bodyElement: ElementsTo.Optional[Body.Value] = Body.element.optional

  // TODO move
  def displayName(store: Store): String = store.names.doFind(Language.Russian.toSpec).name
