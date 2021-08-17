package org.opentorah.pdf

import com.microsoft.playwright.{Browser, Page, Playwright}
//import com.microsoft.playwright.options.Margin

import java.nio.file.Paths
import scala.jdk.CollectionConverters.MapHasAsJava
import scala.util.Using

object PlaywrightPdf {

  def main(args: Array[String]): Unit = Using(create(
    browserPath = Some("/tmp/xxx/"),
    skipBrowserDownload = false
  )) { (playwright: Playwright) =>
    val browser: Browser = playwright.chromium().launch()
    val page: Page = browser.newPage()
    page.navigate("http://www.alter-rebbe.org")
    page.pdf(new Page.PdfOptions()
      .setPath(Paths.get("/tmp/page.pdf"))
//      .setMargin(new Margin()....)
    )
    System.out.println(page.title())
  }

  def create(browserPath: Option[String], skipBrowserDownload: Boolean): Playwright =
    Playwright.create(new Playwright.CreateOptions().setEnv((
      browserPath.toSeq.map(browserPath => "PLAYWRIGHT_BROWSERS_PATH" -> browserPath) ++
      (if (skipBrowserDownload) Seq("PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD" -> "1") else Seq.empty)
    ).toMap.asJava))
}
