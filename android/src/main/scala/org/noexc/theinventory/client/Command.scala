package client

import argonaut.{DecodeJson, Json}
import httpz._
import scalaz.{Free, IList, Inject, NonEmptyList}

sealed abstract class Command[A](val f: String => Request)(implicit val decoder: DecodeJson[A]) extends Product with Serializable {
  val baseURL = "http://10.10.10.127:8081"

  final def request: httpz.Request = requestWithURL(baseURL)

  final def requestWithURL(baseURL: String): httpz.Request = f(baseURL)

  final def actionWithURL(baseURL: String): httpz.Action[A] =
    Core.json[A](requestWithURL(baseURL))(decoder)

  final def action: httpz.Action[A] = actionWithURL(baseURL)
}

object Command {
  private[client] def get(url: String, opt: Config = httpz.emptyConfig): String => Request = {
    baseURL => opt(Request(url = baseURL + url))
  }
}

final case class GetTags() extends Command[IList[Tag]](Command.get("/tags"))
