package org.hoshino9.luogu

import java.net.URLDecoder

import cats.effect.IO
import com.google.gson.{JsonObject, JsonParser}
import okhttp3.{MediaType, OkHttpClient, Request, RequestBody, Response}
import org.hoshino9.luogu.page.LuoGuClientPage

trait LuoGuClient {
  def clientId: Option[String]

  def uid: Option[String]

  def verifyCode(): Array[Byte]

  def login(account: String, password: String, verifyCode: String): Unit

  def get(url: String): Response

  def post(url: String, parameter: JsonObject): Response
}

object LuoGuClient {
  def apply(clientId: Option[String] = None, uid: Option[String] = None): LuoGuClient = {
    val cj = new LuoGuCookieJar
    cj.clientId = clientId
    cj.uid = uid

    val client = new Impl(new OkHttpClient.Builder().cookieJar(cj).build(), cj)
    client.refresh()

    client
  }

  private[luogu] class Impl(private val clientInternal: HttpClient, private val cookieJarInternal: LuoGuCookieJar) extends LuoGuClient with LuoGuClientPage {
    private def csrftoken(): String = {
      val content = get(baseUrl)

      val regex = """<meta name="csrf-token" content="(.+?)">""".r()
      val it = regex.findAllIn(content.body.string())
      it.group(1)
    }

    def request(req: Request): Response = {
      val resp = clientInternal.newCall(req).execute()
      if (resp.isSuccessful) resp else throw new IllegalStateException(resp.body().string())
    }

    override def get(url: String): Response = {
      val req = new Request.Builder()
        .url(url)
        .header("x-luogu-type", "content-only")
        .build()

      request(req)
    }

    override def post(url: String, parameter: JsonObject): Response = {
      val reqBody = RequestBody.create(parameter.toString, MediaType.parse("application/json"))

      val token = csrftoken()
      val req = new Request.Builder()
        .addHeader("x-csrf-token", token)
        .addHeader("referer", baseUrl + "/")
        .url(url)
        .post(reqBody)
        .build()

      request(req)
    }

    override def clientId: Option[String] = cookieJarInternal.clientId

    override def uid: Option[String] = cookieJarInternal.uid

    override def url: String = baseUrl

    override def client: LuoGuClient = this

    override def refresh(): Unit = {
      val content = content()
      val reg = """window\._feInjection = JSON\.parse\(decodeURIComponent\("(.+?)"\)\);""".r()
      val feInjection = reg.findAllIn(content).group(1)
      val obj = JsonParser.parseString(URLDecoder.decode(feInjection, "UTF-8")).getAsJsonObject.getAsJsonObject("currentData")
      this._currentData = Some(obj)
    }

    override def verifyCode(): Array[Byte] = {
      get(s"$baseUrl/api/verify/captcha").body().bytes()
    }

    override def login(account: String, password: String, verifyCode: String): Unit = {
      val obj = new JsonObject
      obj.addProperty("username", account)
      obj.addProperty("password", password)
      obj.addProperty("captcha", verifyCode)

      post(s"$baseUrl/api/auth/userPassLogin", obj)
    }
  }

}