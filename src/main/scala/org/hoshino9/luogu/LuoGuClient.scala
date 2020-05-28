package org.hoshino9.luogu

import java.net.URLDecoder

import okhttp3._
import org.hoshino9.luogu.page.MutableLuoGuClientPage
import play.api.libs.json.{JsObject, Json}

trait LuoGuClient extends MutableLuoGuClientPage {
	def clientId: Option[String]

	def uid: Option[String]

	def verifyCode(): Array[Byte]

	def login(account: String, password: String, verifyCode: String): Unit

	def get(url: String): Response

	def post(url: String, parameter: JsObject): Response
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

	private[luogu] class Impl(private val clientInternal: HttpClient, private val cookieJarInternal: LuoGuCookieJar) extends LuoGuClient {
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

		override def post(url: String, parameter: JsObject): Response = {
			val reqBody = RequestBody.create(Json.stringify(parameter), MediaType.parse("application/json"))

			val token = csrftoken()
			val req = new Request.Builder()
							.url(url)
							.post(reqBody)
							.addHeader("x-csrf-token", token)
							.addHeader("referer", baseUrl + "/")
							.build()

			request(req)
		}

		override def clientId: Option[String] = cookieJarInternal.clientId

		override def uid: Option[String] = cookieJarInternal.uid

		override val url: String = baseUrl

		override val client: LuoGuClient = this

		override def load(): JsObject = {
			val content = this.content
			val reg = """window\._feInjection = JSON\.parse\(decodeURIComponent\("(.+?)"\)\);""".r()
			val feInjection = reg.findAllIn(content).group(1)
			Json.parse(URLDecoder.decode(feInjection, "UTF-8")).as[JsObject]
		}

		override def verifyCode(): Array[Byte] = {
			get(s"$baseUrl/api/verify/captcha").body().bytes()
		}

		override def login(account: String, password: String, verifyCode: String): Unit = {
			val obj = Json.obj(
				"username" -> account,
				"password" -> password,
				"captcha" -> verifyCode
			)

			post(s"$baseUrl/api/auth/userPassLogin", obj)
		}
	}

}