package org.opentorah.pdf

import org.opentorah.util.Reflection
import com.microsoft.playwright.{Browser, Page, Playwright}
import com.microsoft.playwright.impl.{Connection, Driver, PipeTransport, PlaywrightImpl, Transport}
//import com.microsoft.playwright.options.Margin

import java.io.{InputStream, OutputStream}
import java.nio.file.{Path, Paths}
import scala.jdk.CollectionConverters.MapHasAsJava
import scala.util.Using

object PlaywrightPdf {

  def main(args: Array[String]): Unit = Using(create(
    browserPath = Some("/tmp/xxx/"),
    skipBrowserDownload = true
  )) { playwright: Playwright =>
    val browser: Browser = playwright.chromium().launch()
    val page: Page = browser.newPage()
    page.navigate("http://www.alter-rebbe.org")
    page.pdf(new Page.PdfOptions()
      .setPath(Paths.get("/tmp/page.pdf"))
//      .setMargin(new Margin()....)
    )
    System.out.println(page.title())
  }

  def create(browserPath: Option[String], skipBrowserDownload: Boolean): Playwright = create((
    browserPath.toSeq.map(browserPath => "PLAYWRIGHT_BROWSERS_PATH" -> browserPath) ++
    (if (skipBrowserDownload) Seq("PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD" -> "1") else Seq.empty)
  ).toMap)

  def create(environment: Map[String, String]): Playwright = {
    val driver: Path = Driver.ensureDriverInstalled()
    val pb: ProcessBuilder = new ProcessBuilder(driver.toString, "run-driver")
    pb.redirectError(ProcessBuilder.Redirect.INHERIT)

    pb.environment().putAll(environment.asJava)

    val p: Process = pb.start()

    val pipeTransport: PipeTransport = Reflection.newInstance(classOf[PipeTransport],
      classOf[InputStream] -> p.getInputStream,
      classOf[OutputStream] -> p.getOutputStream
    )

    val connection: Connection = Reflection.newInstance(classOf[Connection],
      classOf[Transport] -> pipeTransport
    )

    val result: PlaywrightImpl = connection.waitForObjectWithKnownName("Playwright").asInstanceOf[PlaywrightImpl]

    Reflection.set(result, "driverProcess", p)
    Reflection.invoke(result, "initSharedSelectors", classOf[PlaywrightImpl] -> null)

    result
  }

//  def create(environment: Map[String, String]): Playwright = {
//    Playwright.create()
//  }
}
