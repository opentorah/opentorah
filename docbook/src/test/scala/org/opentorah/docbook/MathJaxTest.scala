package org.opentorah.docbook

import org.opentorah.fop.Svg
import org.opentorah.math.MathJaxRunner
import org.opentorah.xml.{ScalaXml, XLink}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class MathJaxTest extends AnyFlatSpecLike, Matchers:

  private def test(
    name: String,
    tex: String,
    svgExpected: String
  ): Unit =
    def test(useJ2V8: Boolean): Unit =
      val project = PluginTestProject(
        prefix = "mathJaxTestProjects",
        name = s"$name-useJ2V8-$useJ2V8",
        document = DocBook.prettyPrinter.renderWithHeader(ScalaXml)(
          <article xmlns={DocBook.namespace.uri} version={DocBook.version}>
            <para>{s"$$$tex$$"}</para>
          </article>
        ),
        isPdfEnabled = true,
        isMathJaxEnabled = true,
        useJ2V8 = useJ2V8
      )

      val log: String = project.run(logInfo = true)
      substring(log, MathJaxRunner.logStart, MathJaxRunner.logSep) shouldBe tex
      substring(log, MathJaxRunner.logSep, MathJaxRunner.logEnd) shouldBe svgExpected

///    test(useJ2V8 = true)
    test(useJ2V8 = false)

  private def substring(string: String, from: String, to: String): String =
    val result: String = string.substring(string.indexOf(from) + from.length)
    result.substring(0, result.indexOf(to))

  it should "typeset E = mc^2" in test("emc2", "E = mc^2",
     s"""|<svg xmlns:xlink="${XLink.namespace.uri}" width="9.155ex" height="3.009ex" style="vertical-align: -0.505ex;" viewBox="0 -1078.4 3941.5 1295.7" role="img" focusable="false" xmlns="${Svg.namespace.uri}">
         |<defs>
         |<path stroke-width="1" id="E1-MJMATHI-45" d="M492 213Q472 213 472 226Q472 230 477 250T482 285Q482 316 461 323T364 330H312Q311 328 277 192T243 52Q243 48 254 48T334 46Q428 46 458 48T518 61Q567 77 599 117T670 248Q680 270 683 272Q690 274 698 274Q718 274 718 261Q613 7 608 2Q605 0 322 0H133Q31 0 31 11Q31 13 34 25Q38 41 42 43T65 46Q92 46 125 49Q139 52 144 61Q146 66 215 342T285 622Q285 629 281 629Q273 632 228 634H197Q191 640 191 642T193 659Q197 676 203 680H757Q764 676 764 669Q764 664 751 557T737 447Q735 440 717 440H705Q698 445 698 453L701 476Q704 500 704 528Q704 558 697 578T678 609T643 625T596 632T532 634H485Q397 633 392 631Q388 629 386 622Q385 619 355 499T324 377Q347 376 372 376H398Q464 376 489 391T534 472Q538 488 540 490T557 493Q562 493 565 493T570 492T572 491T574 487T577 483L544 351Q511 218 508 216Q505 213 492 213Z"></path>
         |<path stroke-width="1" id="E1-MJMAIN-3D" d="M56 347Q56 360 70 367H707Q722 359 722 347Q722 336 708 328L390 327H72Q56 332 56 347ZM56 153Q56 168 72 173H708Q722 163 722 153Q722 140 707 133H70Q56 140 56 153Z"></path>
         |<path stroke-width="1" id="E1-MJMATHI-6D" d="M21 287Q22 293 24 303T36 341T56 388T88 425T132 442T175 435T205 417T221 395T229 376L231 369Q231 367 232 367L243 378Q303 442 384 442Q401 442 415 440T441 433T460 423T475 411T485 398T493 385T497 373T500 364T502 357L510 367Q573 442 659 442Q713 442 746 415T780 336Q780 285 742 178T704 50Q705 36 709 31T724 26Q752 26 776 56T815 138Q818 149 821 151T837 153Q857 153 857 145Q857 144 853 130Q845 101 831 73T785 17T716 -10Q669 -10 648 17T627 73Q627 92 663 193T700 345Q700 404 656 404H651Q565 404 506 303L499 291L466 157Q433 26 428 16Q415 -11 385 -11Q372 -11 364 -4T353 8T350 18Q350 29 384 161L420 307Q423 322 423 345Q423 404 379 404H374Q288 404 229 303L222 291L189 157Q156 26 151 16Q138 -11 108 -11Q95 -11 87 -5T76 7T74 17Q74 30 112 181Q151 335 151 342Q154 357 154 369Q154 405 129 405Q107 405 92 377T69 316T57 280Q55 278 41 278H27Q21 284 21 287Z"></path>
         |<path stroke-width="1" id="E1-MJMATHI-63" d="M34 159Q34 268 120 355T306 442Q362 442 394 418T427 355Q427 326 408 306T360 285Q341 285 330 295T319 325T330 359T352 380T366 386H367Q367 388 361 392T340 400T306 404Q276 404 249 390Q228 381 206 359Q162 315 142 235T121 119Q121 73 147 50Q169 26 205 26H209Q321 26 394 111Q403 121 406 121Q410 121 419 112T429 98T420 83T391 55T346 25T282 0T202 -11Q127 -11 81 37T34 159Z"></path>
         |<path stroke-width="1" id="E1-MJMAIN-32" d="M109 429Q82 429 66 447T50 491Q50 562 103 614T235 666Q326 666 387 610T449 465Q449 422 429 383T381 315T301 241Q265 210 201 149L142 93L218 92Q375 92 385 97Q392 99 409 186V189H449V186Q448 183 436 95T421 3V0H50V19V31Q50 38 56 46T86 81Q115 113 136 137Q145 147 170 174T204 211T233 244T261 278T284 308T305 340T320 369T333 401T340 431T343 464Q343 527 309 573T212 619Q179 619 154 602T119 569T109 550Q109 549 114 549Q132 549 151 535T170 489Q170 464 154 447T109 429Z"></path>
         |</defs>
         |<g stroke="currentColor" fill="currentColor" stroke-width="0" transform="matrix(1 0 0 -1 0 0)">
         | <use xlink:href="#E1-MJMATHI-45" x="0" y="0"></use>
         | <use xlink:href="#E1-MJMAIN-3D" x="1042" y="0"></use>
         | <use xlink:href="#E1-MJMATHI-6D" x="2098" y="0"></use>
         |<g transform="translate(2977,0)">
         | <use xlink:href="#E1-MJMATHI-63" x="0" y="0"></use>
         | <use transform="scale(0.861)" xlink:href="#E1-MJMAIN-32" x="503" y="421"></use>
         |</g>
         |</g>
         |</svg>""".stripMargin
    )

  it should "typeset quadratic" in test("quadratic", "x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.",
     s"""|<svg xmlns:xlink="${XLink.namespace.uri}" width="18.429ex" height="7.009ex" style="vertical-align: -1.755ex;" viewBox="0 -2262.4 7934.7 3017.9" role="img" focusable="false" xmlns="${Svg.namespace.uri}">
         |<defs>
         |<path stroke-width="1" id="E1-MJMATHI-78" d="M52 289Q59 331 106 386T222 442Q257 442 286 424T329 379Q371 442 430 442Q467 442 494 420T522 361Q522 332 508 314T481 292T458 288Q439 288 427 299T415 328Q415 374 465 391Q454 404 425 404Q412 404 406 402Q368 386 350 336Q290 115 290 78Q290 50 306 38T341 26Q378 26 414 59T463 140Q466 150 469 151T485 153H489Q504 153 504 145Q504 144 502 134Q486 77 440 33T333 -11Q263 -11 227 52Q186 -10 133 -10H127Q78 -10 57 16T35 71Q35 103 54 123T99 143Q142 143 142 101Q142 81 130 66T107 46T94 41L91 40Q91 39 97 36T113 29T132 26Q168 26 194 71Q203 87 217 139T245 247T261 313Q266 340 266 352Q266 380 251 392T217 404Q177 404 142 372T93 290Q91 281 88 280T72 278H58Q52 284 52 289Z"></path>
         |<path stroke-width="1" id="E1-MJMAIN-3D" d="M56 347Q56 360 70 367H707Q722 359 722 347Q722 336 708 328L390 327H72Q56 332 56 347ZM56 153Q56 168 72 173H708Q722 163 722 153Q722 140 707 133H70Q56 140 56 153Z"></path>
         |<path stroke-width="1" id="E1-MJMAIN-2212" d="M84 237T84 250T98 270H679Q694 262 694 250T679 230H98Q84 237 84 250Z"></path>
         |<path stroke-width="1" id="E1-MJMATHI-62" d="M73 647Q73 657 77 670T89 683Q90 683 161 688T234 694Q246 694 246 685T212 542Q204 508 195 472T180 418L176 399Q176 396 182 402Q231 442 283 442Q345 442 383 396T422 280Q422 169 343 79T173 -11Q123 -11 82 27T40 150V159Q40 180 48 217T97 414Q147 611 147 623T109 637Q104 637 101 637H96Q86 637 83 637T76 640T73 647ZM336 325V331Q336 405 275 405Q258 405 240 397T207 376T181 352T163 330L157 322L136 236Q114 150 114 114Q114 66 138 42Q154 26 178 26Q211 26 245 58Q270 81 285 114T318 219Q336 291 336 325Z"></path>
         |<path stroke-width="1" id="E1-MJMAIN-B1" d="M56 320T56 333T70 353H369V502Q369 651 371 655Q376 666 388 666Q402 666 405 654T409 596V500V353H707Q722 345 722 333Q722 320 707 313H409V40H707Q722 32 722 20T707 0H70Q56 7 56 20T70 40H369V313H70Q56 320 56 333Z"></path>
         |<path stroke-width="1" id="E1-MJMAIN-32" d="M109 429Q82 429 66 447T50 491Q50 562 103 614T235 666Q326 666 387 610T449 465Q449 422 429 383T381 315T301 241Q265 210 201 149L142 93L218 92Q375 92 385 97Q392 99 409 186V189H449V186Q448 183 436 95T421 3V0H50V19V31Q50 38 56 46T86 81Q115 113 136 137Q145 147 170 174T204 211T233 244T261 278T284 308T305 340T320 369T333 401T340 431T343 464Q343 527 309 573T212 619Q179 619 154 602T119 569T109 550Q109 549 114 549Q132 549 151 535T170 489Q170 464 154 447T109 429Z"></path>
         |<path stroke-width="1" id="E1-MJMAIN-34" d="M462 0Q444 3 333 3Q217 3 199 0H190V46H221Q241 46 248 46T265 48T279 53T286 61Q287 63 287 115V165H28V211L179 442Q332 674 334 675Q336 677 355 677H373L379 671V211H471V165H379V114Q379 73 379 66T385 54Q393 47 442 46H471V0H462ZM293 211V545L74 212L183 211H293Z"></path>
         |<path stroke-width="1" id="E1-MJMATHI-61" d="M33 157Q33 258 109 349T280 441Q331 441 370 392Q386 422 416 422Q429 422 439 414T449 394Q449 381 412 234T374 68Q374 43 381 35T402 26Q411 27 422 35Q443 55 463 131Q469 151 473 152Q475 153 483 153H487Q506 153 506 144Q506 138 501 117T481 63T449 13Q436 0 417 -8Q409 -10 393 -10Q359 -10 336 5T306 36L300 51Q299 52 296 50Q294 48 292 46Q233 -10 172 -10Q117 -10 75 30T33 157ZM351 328Q351 334 346 350T323 385T277 405Q242 405 210 374T160 293Q131 214 119 129Q119 126 119 118T118 106Q118 61 136 44T179 26Q217 26 254 59T298 110Q300 114 325 217T351 328Z"></path>
         |<path stroke-width="1" id="E1-MJMATHI-63" d="M34 159Q34 268 120 355T306 442Q362 442 394 418T427 355Q427 326 408 306T360 285Q341 285 330 295T319 325T330 359T352 380T366 386H367Q367 388 361 392T340 400T306 404Q276 404 249 390Q228 381 206 359Q162 315 142 235T121 119Q121 73 147 50Q169 26 205 26H209Q321 26 394 111Q403 121 406 121Q410 121 419 112T429 98T420 83T391 55T346 25T282 0T202 -11Q127 -11 81 37T34 159Z"></path>
         |<path stroke-width="1" id="E1-MJSZ2-221A" d="M1001 1150Q1017 1150 1020 1132Q1020 1127 741 244L460 -643Q453 -650 436 -650H424Q423 -647 423 -645T421 -640T419 -631T415 -617T408 -594T399 -560T385 -512T367 -448T343 -364T312 -259L203 119L138 41L111 67L212 188L264 248L472 -474L983 1140Q988 1150 1001 1150Z"></path>
         |<path stroke-width="1" id="E1-MJMAIN-2E" d="M78 60Q78 84 95 102T138 120Q162 120 180 104T199 61Q199 36 182 18T139 0T96 17T78 60Z"></path>
         |</defs>
         |<g stroke="currentColor" fill="currentColor" stroke-width="0" transform="matrix(1 0 0 -1 0 0)">
         | <use xlink:href="#E1-MJMATHI-78" x="0" y="0"></use>
         | <use xlink:href="#E1-MJMAIN-3D" x="850" y="0"></use>
         |<g transform="translate(1906,0)">
         |<g transform="translate(120,0)">
         |<rect stroke="none" width="5509" height="60" x="0" y="220"></rect>
         |<g transform="translate(60,932)">
         | <use transform="scale(0.861)" xlink:href="#E1-MJMAIN-2212" x="0" y="0"></use>
         | <use transform="scale(0.861)" xlink:href="#E1-MJMATHI-62" x="778" y="0"></use>
         | <use transform="scale(0.861)" xlink:href="#E1-MJMAIN-B1" x="1208" y="0"></use>
         |<g transform="translate(1710,0)">
         | <use transform="scale(0.861)" xlink:href="#E1-MJSZ2-221A" x="0" y="162"></use>
         |<rect stroke="none" width="2817" height="51" x="861" y="1079"></rect>
         |<g transform="translate(861,0)">
         | <use transform="scale(0.861)" xlink:href="#E1-MJMATHI-62" x="0" y="0"></use>
         | <use transform="scale(0.861)" xlink:href="#E1-MJMAIN-32" x="429" y="308"></use>
         | <use transform="scale(0.861)" xlink:href="#E1-MJMAIN-2212" x="1030" y="0"></use>
         | <use transform="scale(0.861)" xlink:href="#E1-MJMAIN-34" x="1808" y="0"></use>
         | <use transform="scale(0.861)" xlink:href="#E1-MJMATHI-61" x="2309" y="0"></use>
         | <use transform="scale(0.861)" xlink:href="#E1-MJMATHI-63" x="2838" y="0"></use>
         |</g>
         |</g>
         |</g>
         |<g transform="translate(2311,-587)">
         | <use transform="scale(0.861)" xlink:href="#E1-MJMAIN-32" x="0" y="0"></use>
         | <use transform="scale(0.861)" xlink:href="#E1-MJMATHI-61" x="500" y="0"></use>
         |</g>
         |</g>
         |</g>
         | <use xlink:href="#E1-MJMAIN-2E" x="7656" y="0"></use>
         |</g>
         |</svg>""".stripMargin
  )
