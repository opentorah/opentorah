package org.opentorah.calendar.service

import org.opentorah.metadata.Language
import org.opentorah.service.ServiceApp
import zio.http.Request
import zio.http.codec.{HttpCodec, HttpCodecType}

object Lang:
  val queryParameterName: String = "lang"
  
  val codec: HttpCodec[HttpCodecType.Query, Language.Spec] = HttpCodec
    .query(queryParameterName)
    .optional
    .transform[Language.Spec](fromParameter)((value: Language.Spec) => Some(value.language.get.toString))

  def fromRequest(request: Request): Language.Spec =
    fromParameter(ServiceApp.queryParameter(request, queryParameterName))

  def fromParameter(parameter: Option[String]): Language.Spec =
    parameter.map(Language.getForName).getOrElse(Language.English).toSpec
    