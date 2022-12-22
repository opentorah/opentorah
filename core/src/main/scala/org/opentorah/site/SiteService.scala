package org.opentorah.site

import org.opentorah.service.ServiceApp
import ServiceApp.given
import org.opentorah.files.GoogleCloudStorageSynchronizer
import org.opentorah.store.{Path, Pure, Store}
import org.opentorah.tei.LinksResolver
import org.opentorah.util.{Effects, Files, Strings}
import org.opentorah.xml.{Caching, Element, From, Parsable, Parser, Unparser}
import org.slf4j.Logger
import zio.http.{!!, /, Body, Http, HttpApp, Request, Response, *} // TODO remove * - what in it makes matching Methods possible?
import zio.http.model.{Headers, HeaderValues, HTTP_CHARSET, Method}
import zio.{Chunk, Task, ZIO}
import java.io.File
import java.net.URL

abstract class SiteService[S <: Site] extends Element[S]("site"), ServiceApp:
  final override protected def log: Logger = Site.logger

  // TODO does this belong in the base class?
  protected def bucketName: String

  final def readSite(url: String): Task[S] = readSite(Files.string2url(url))

  private final def readSite(url: URL): Task[S] =
    val siteFileUrl: URL = Files.fileInDirectory(url, "site.xml")
    val result: Parser[S] = for
      _ <- Effects.effect(logger.info(s"Reading site from $siteFileUrl"))
      // TODO abstract over Xml?
      result: S <- parse(From.url(siteFileUrl))
      _ <- Effects.effect(logger.info(s"Reading site from $siteFileUrl - done"))
    yield result
    Parser.toTask(result)

  final override protected def run(args: Chunk[String]): ZIO[Any, Throwable, Any] =
    if args.isEmpty then serve(urlString = None) else args.head match
      case "serveRemoteSite"         => serve(urlString = None)
      case "serveSite"               => serve(urlString = args.lift(1))
      case "buildAndPrettyPrintSite" => build(prettyPrint = true , args.drop(1))
      case "buildSite"               => build(prettyPrint = false, args.drop(1))
      case "uploadSite"              => upload(dryRun = false, args.drop(1))
      case "uploadSiteDryRun"        => upload(dryRun = true , args.drop(1))

  private def build(prettyPrint: Boolean, args: Chunk[String]): Task[Unit] =
    val directoryPath: String = args(0)
    val globalSubstitutions: Map[String, String] = args.lift(1).fold(Map.empty)(Strings.toMap)
    for
      site: S <- readSite(Files.file2url(File(directoryPath)))
      _ <- site.build(prettyPrint, globalSubstitutions)
    yield ()

  private def upload(dryRun: Boolean, args: Chunk[String]): Task[Unit] =
    val directoryPath: String = args(0)
    val serviceAccountKey: String = args(1)
    ZIO.succeedBlocking(GoogleCloudStorageSynchronizer(
      serviceAccountKey = serviceAccountKey,
      bucketName = bucketName,
      bucketPrefix = "",
      directoryPath = directoryPath + "/",
      dryRun = dryRun
    ).sync())

  private def serve(urlString: Option[String]): ZIO[Any, Throwable, Any] =
    val siteUrl: String = if urlString.nonEmpty then
      val result: String = urlString.get
      logger.info(s"siteUri argument supplied: $result")
      result
    else getParameter("STORE", s"http://$bucketName/") // TODO switch to https

    serve(routes(siteUrl))

  private def routes(siteUrl: String): HttpApp[Any, Throwable] =
    var cachedSite: Option[S] = None
    def getSite: Task[S] = cachedSite.map(ZIO.succeed(_)).getOrElse(readSite(siteUrl).map(result =>
      cachedSite = Some(result)
      result
    ))

    Effects.unsafeRun(getSite)

    def reset(request: Request): Task[Response] = for
      _ <- ZIO.succeedBlocking({
        logger.info(request, "RST")
        cachedSite = None
      })
      _ <- getSite
    yield Response(
      headers = Headers.contentType(HeaderValues.textPlain),
      body = Body.fromString("Site reset!")
    )

    def get(request: Request): ZIO[Any, Nothing, Response] =
      val pathString: String = request.url.path.toString
      val path: Seq[String] = Files.splitAndDecodeUrl(pathString)

      ServiceApp.orNotFound(pathString, getSite.flatMap(site =>
        if site.isStatic(path) then ServiceApp.staticFile(
          url = Files.pathUnder(Files.string2url(siteUrl), pathString),
          request = Some(request)
        ) else site.getResponse(pathString).map(siteResponse =>
          val bytes: Array[Byte] = siteResponse.content.getBytes(HTTP_CHARSET)

          Response(
            headers =
              // TODO: `Content-Type`(MediaType.unsafeParse(siteResponse.mimeType), Charset.`UTF-8`)
              // TODO more headers!
              Headers.contentType(siteResponse.mimeType) ++
              Headers.contentLength(bytes.length.toLong),
            body = Body.fromStream(zio.stream.ZStream.fromChunk(Chunk.fromArray(bytes)))
          )
        )
      ))

    Http.collectZIO[Request] {
      case request@Method.GET -> !! / "reset-cached-site" => reset(request)
      case request => timed(get)(request)
    }

// Note: this is on the top level and not nested in the companion object,
// *and* is not the companion object (TODO: try this again!)
// since in those cases Scala compiler does not generate static main()...
object SiteServiceCommon extends SiteService[Site]:
  override def projectId: String = "???"
  override def bucketName: String = "???"

  override def contentParsable: Parsable[Site] = new Parsable[Site]:
    override def unparser: Unparser[Site] = Unparser.concat[Site](
      SiteCommon.required(_.common),
    )

    override def parser: Parser[Site] = for
      fromUrl: Element.FromUrl <- Element.fromUrl
      common: SiteCommon <- SiteCommon.required()
    yield new Site(fromUrl, common) with Pure[?]:
      override def storesPure: Seq[Store] = Seq.empty
      override def content(path: Path, extension: Option[String]): Caching.Parser[Site.Response] = ???
      override def pathShortener: Caching.Parser[Path.Shortener] = ZIO.succeed(identity)
      override def path(store: Store): Path = Seq.empty
      override protected def linkResolver(path: Path, pathShortener: Path.Shortener): LinksResolver = LinksResolver.empty
