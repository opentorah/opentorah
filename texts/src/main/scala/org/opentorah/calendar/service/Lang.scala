package org.opentorah.calendar.service

import org.opentorah.metadata.Language
import zio.http.Request
import zio.http.codec.{HttpCodec, HttpCodecType}

object Lang:
  val queryParameterName: String = "lang"
  
  val codec: HttpCodec[HttpCodecType.Query, Language.Spec] = HttpCodec
    .query[String](queryParameterName)
    .optional
    .transform[Language.Spec](fromParameter)((value: Language.Spec) => Some(value.language.get.toString))

  def fromRequest(request: Request): Language.Spec =
    fromParameter(request.url.queryParams.queryParam(queryParameterName))

  def fromParameter(parameter: Option[String]): Language.Spec =
    parameter.map(Language.getForName).getOrElse(Language.English).toSpec
    