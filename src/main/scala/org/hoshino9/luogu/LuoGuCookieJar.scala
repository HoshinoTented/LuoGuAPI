package org.hoshino9.luogu

import java.util

import okhttp3.{Cookie, CookieJar, HttpUrl}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

class LuoGuCookieJar extends CookieJar {
  var clientId: Option[String] = None
  var uid: Option[String] = None

  override def loadForRequest(httpUrl: HttpUrl): util.List[Cookie] = {
    if (httpUrl.host().endsWith(host)) {
      var cookies = Seq[Cookie]()

      for (id <- clientId) {
        cookies = cookies :+ LuoGuCookieJar.buildCookie("__client_id", id)
      }

      for (id <- uid) {
        cookies = cookies :+ LuoGuCookieJar.buildCookie("_uid", id)
      }

      cookies.asJava
    } else Seq().asJava
  }

  override def saveFromResponse(httpUrl: HttpUrl, list: util.List[Cookie]): Unit = {
    if (httpUrl.host().endsWith(host)) {
      list.forEach { elem =>
        if (elem.name() == "__client_id") {
          this.clientId = Some(elem.value())
        }

        if (elem.name() == "_uid") {
          this.uid = Some(elem.value())
        }
      }
    }
  }
}

object LuoGuCookieJar {
  private def buildCookie(name: String, value: String): Cookie = {
    new Cookie.Builder().domain(host).name(name).value(value).build()
  }
}