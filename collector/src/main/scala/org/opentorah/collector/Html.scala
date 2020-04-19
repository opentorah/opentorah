package org.opentorah.collector

import scala.xml.Elem

object Html {

  // TODO add  <!DOCTYPE html>
  // Note: meta and link elements are closed to make Scala XML happy; I think this should work in the browsers :)
  // Skipped {seo} after metas and feedMeta before googleAnalytics - I do not see them in Minima sources *and*
  //   I do not use them :)
  // I think font-awesome is only used for blog post tags, and I am not doing the blog on the dynamic site?
  def defaultLayout(
    content: Elem,
    lang: String = "en",
    style: String = "main",
    googleAnalyticsId: Option[String] = None,
    faviconJpeg: String = "alter-rebbe",
    author: String,
    email: String,
    footerCol3: Elem = footerCol3default,
    windowName: Option[String] = None
  ): Elem =
    <html lang={lang}>
      <head>
        <meta charset="utf-8"/>
        <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        <link rel="stylesheet" href={s"/assets/$style.css"}/>
        {googleAnalyticsId.fold[Seq[Elem]](Seq.empty){ googleAnalyticsId =>
          Seq(<script>{googleAnalytics(googleAnalyticsId)}</script>)}}
        <link rel="icon" type="image/jpeg" href={s"/$faviconJpeg.jpeg"}/>
        <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
              integrity="sha384-wvfXpqpZZVQGK6TAh5PVlGOfQNHSoD2xbE+QkPxCAFlNEevoEH3Sl0sibVcOQVnN" crossorigin="anonymous"/>
      </head>
      <body>
        {header}
        <main class="page-content" aria-label="Content">
          <div class="wrapper">{content}</div>
        </main>
        {footer(author, email, footerCol3)}
      </body>
      {windowName.fold[Seq[Elem]](Seq.empty){ windowName =>
        Seq(<script type="module">{s"""window.name = "$windowName"""}</script>)}}
    </html>

  // TODO move out of here :)
  def footerCol3default: Elem =
    <p>
      documents related to early Chabad history<br/>
      licensed under <a href="http://creativecommons.org/licenses/by/4.0/" target="collectionViewer">CC BY 4.0</a> by
      <a href="http://www.opentorah.org/" target="collectionViewer">Open Torah</a>
    </p>

  // Only in production :)
  def googleAnalytics(id: String): String =
    s"""
       |if(!(window.doNotTrack === "1" || navigator.doNotTrack === "1" || navigator.doNotTrack === "yes" || navigator.msDoNotTrack === "1")) {
       |  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
       |  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
       |  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
       |  })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
       |
       |  ga('create', '$id', 'auto');
       |  ga('send', 'pageview');
       |}
       |""".stripMargin

  def header: Elem = ??? // TODO header.html

  // TODO u-url is a relative URL of the page?
  def footer(
    author: String,
    email: String,
    col3: Elem
  ): Elem =
    <footer class="site-footer h-card">
      <data class="u-url" href="/"/>

      <div class="wrapper">
        <div class="footer-col-wrapper">
          <div class="footer-col footer-col-1">
            <ul class="contact-list">
              <li class="p-name">{author}</li>
              <li><a class="u-email" href={s"mailto:$email"}>{email}</a></li>
            </ul>
          </div>
          <div class="footer-col footer-col-2">{social}</div>
          <div class="footer-col footer-col-3">{col3}</div>
        </div>
      </div>
    </footer>

  def social: Seq[Elem] = Seq.empty
}
