@file:Suppress("unused", "UNUSED_PARAMETER")

package org.hoshino9.luogu

import com.google.gson.JsonNull
import com.google.gson.JsonObject
import io.ktor.client.call.receive
import io.ktor.client.features.ClientRequestException
import io.ktor.client.features.cookies.cookies
import io.ktor.client.request.get
import io.ktor.client.request.header
import io.ktor.client.request.request
import io.ktor.client.response.HttpResponse
import io.ktor.client.response.readBytes
import io.ktor.http.Cookie
import io.ktor.http.HttpMethod
import io.ktor.http.Url
import io.ktor.http.isSuccess
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.baseUrl
import org.hoshino9.luogu.domain
import org.hoshino9.luogu.page.DeprecatedLuoGuPage
import org.hoshino9.luogu.user.LoggedUserImpl
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup

interface LuoGuClient {
	companion object {
		operator fun invoke(): LuoGuClient {
			return Impl(defaultClient)
		}

		operator fun invoke(clientId: String, uid: Int): LuoGuClient {
			val url = Url(baseUrl)

			return Impl(specifiedCookieClient(
					listOf(
							url to Cookie("_uid", uid.toString(), domain = domain),
							url to Cookie("__client_id", clientId, domain = domain)
					)
			))
		}

		class Impl(client: HttpClient) : LuoGuClient, DeprecatedLuoGuPage(client) {
			init {
				refresh()
			}

			override val url: String = baseUrl

			private suspend fun csrfToken(): String = csrfTokenFromPage(page())

			private suspend fun HttpResponse.assert(): ByteArray {
				return if (status.isSuccess()) {
					readBytes()
				} else throw IllegalStatusCodeException(status.value, strData())
			}

			override suspend fun get(url: String): ByteArray {
				val resp = client.request<HttpResponse>(url) {
					header("x-luogu-type", "content-only")
				}

				return resp.assert()
			}

			override suspend fun post(url: String, body: JsonObject): ByteArray {
				val resp = client.request<HttpResponse>(url) {
					this.body = body.asParams
					method = HttpMethod.Post
					header("referer", baseUrl)
					header("x-csrf-token", csrfToken())
				}

				return resp.assert()
			}

			override val cookieUid: String?
				get() = runBlocking {
					client.cookies(baseUrl).firstOrNull { it.name == "_uid" }?.value
				}

			override val cookieClientId: String?
				get() = runBlocking {
					client.cookies(baseUrl).firstOrNull { it.name == "__client_id" }?.value
				}

			override suspend fun verifyCode(): ByteArray {
				return get("$baseUrl/api/verify/captcha")
			}

			override suspend fun login(form: LoginForm) {
				val body = Deserializable.gson.toJsonTree(form).asJsonObject

				post("$baseUrl/api/auth/userPassLogin", body)
				refresh()
			}

			override suspend fun logout(): Boolean {
				get("$baseUrl/api/auth/logout?uid=${cookieUid}")
				refresh()

				return true
			}
		}
	}

	data class LoginForm(val username: String, val password: String, val verifyCode: String)

	val cookieUid: String?
	val cookieClientId: String?

	suspend fun verifyCode(): ByteArray
	suspend fun login(form: LoginForm)
	suspend fun logout(): Boolean

	suspend fun get(url: String): ByteArray
	suspend fun post(url: String, body: JsonObject): ByteArray
}

/**
 * # LuoGu
 * **你谷**客户端类，目前仍然是 *过时页面*，等待洛谷官方更新。
 *
 * 洛谷代表了一个客户端，所有获取数据的函数都应该是 LuoGu 的扩展函数。
 *
 * ## 登陆
 *
 * [verifyCode] 用于获取验证码（验证码不仅仅用于登录）， [login] 则用于登录，需要验证码。
 */
@Deprecated("Bad design", ReplaceWith("LuoGuClient"))
@Suppress("MemberVisibilityCanBePrivate")
class LuoGu constructor(client: HttpClient = defaultClient) : DeprecatedLuoGuPage(client) {
	companion object {
		@JvmName("fromCookie")
		operator fun invoke(clientId: String, uid: Int): LuoGu {
			val url = Url(baseUrl)

			return LuoGu(
					specifiedCookieClient(
							listOf(
									url to Cookie("_uid", uid.toString(), domain = domain),
									url to Cookie("__client_id", clientId, domain = domain)
							)
					)
			)
		}
	}

	override val url: String = baseUrl

	init {
		runBlocking {
			refresh()
		}
	}

	val uid: Cookie
		get() {
			return runBlocking {
				client.cookies(baseUrl).first {
					it.name == "_uid"
				}
			}
		}

	val clientId: Cookie
		get() {
			return runBlocking {
				client.cookies(baseUrl).first {
					it.name == "__client_id"
				}
			}
		}

	/**
	 * 一个奇怪的Token, 似乎十分重要, 大部分操作都需要这个
	 */
	suspend fun csrfToken(): String {
		return csrfTokenFromPage(Jsoup.parse(client.get(url)))
	}

	/**
	 * 是否已登录
	 */
	val isLogged: Boolean
		get() {
			return feInjection["currentUser"] !is JsonNull
		}

	/**
	 * 是否需要解锁
	 * @return 返回解锁 mode (2fa 代表两步验证 secret 代表密码)
	 */
	val needUnlock: String?
		get() {
			return feInjection["currentData"].asJsonObject["mode"]?.asString
		}

	/**
	 * 获取验证码
	 */
	suspend fun verifyCode(): ByteArray {
		return client.get("$baseUrl/api/verify/captcha")
	}

	/**
	 * 解锁
	 * 两步验证和密码解锁通用
	 * @see needUnlock
	 */
	suspend fun unlock(code: String): String {
		val params = JsonObject().apply { addProperty("code", code) }

		return apiPost("api/auth/unlock") {
			referer("auth/unlock")
			body = params.asParams
		}.receive<String>().also {
			refresh()
		}
	}

	/**
	 * 登录**你谷**
	 *
	 * @param account 账号
	 * @param password 密码
	 * @param verifyCode 验证码, 通过 [LuoGu.verifyCode] 获得
	 *
	 * @throws ClientRequestException
	 *
	 * @see LuoGu.verifyCode
	 * @see LoggedUserImpl
	 */
	suspend fun login(account: String, password: String, verifyCode: String) {
		val json = JsonObject().apply {
			addProperty("username", account)
			addProperty("password", password)
			addProperty("captcha", verifyCode)
		}

		apiPost("api/auth/userPassLogin") {
			referer("auth/login")
			body = json.asParams
		}.receive<String>()

		refresh()
	}

	/**
	 * 登出
	 *
	 * @throws ClientRequestException
	 */
	suspend fun logout() {
		client.get<String>("$baseUrl/api/auth/logout?uid=${uid.value}") {
			referer("")
		}

		refresh()
	}
}