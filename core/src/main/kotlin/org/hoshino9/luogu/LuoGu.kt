@file:Suppress("unused", "UNUSED_PARAMETER")

package org.hoshino9.luogu

import com.google.gson.JsonNull
import com.google.gson.JsonObject
import io.ktor.client.call.receive
import io.ktor.client.features.cookies.cookies
import io.ktor.client.request.get
import io.ktor.http.Cookie
import io.ktor.http.Url
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.DeprecatedLuoGuPage
import org.hoshino9.luogu.user.LoggedUser
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import java.io.OutputStream

/**
 * # LuoGU
 * **你谷**客户端类
 */
@Suppress("MemberVisibilityCanBePrivate")
open class LuoGu @JvmOverloads constructor(client: HttpClient = defaultClient) : DeprecatedLuoGuPage(client) {
	companion object {
		@JvmName("newInstance")
		operator fun invoke(clientId: String, uid: String): LuoGu {
			val url = Url(baseUrl)

			return LuoGu(
					specifiedCookieClient(
							listOf(
									url to Cookie("_uid", uid),
									url to Cookie("__client_id", clientId))
					)
			)
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

	override val url: String = LuoGuUtils.baseUrl

	/**
	 * 一个奇怪的Token, 似乎十分重要, 大部分操作都需要这个
	 */
	val csrfToken: String
		get() {
			return runBlocking {
				LuoGuUtils.csrfTokenFromPage(Jsoup.parse(client.get(url)))
			}
		}

	/**
	 * 是否已登录
	 */
	val isLogged: Boolean
		get() {
			return feInjection.get("currentUser") != JsonNull.INSTANCE
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
	 * @param out 输出流, 将会把验证码**图片**输出到这个流里
	 */
	suspend fun verifyCode(out: OutputStream) {
		val image: ByteArray = client.get("$baseUrl/api/verify/captcha")
		out.write(image)
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
		}.receive()
	}

	/**
	 * 登录**你谷**
	 * @param account 账号
	 * @param password 密码
	 * @param verifyCode 验证码, 通过 [LuoGu.verifyCode] 获得
	 * @throws IllegalStatusCodeException 当登录失败时抛出
	 * @throws IllegalStatusCodeException 当请求码错误时抛出
	 *
	 * @see LuoGu.verifyCode
	 * @see LoggedUser
	 * @see IllegalStatusCodeException
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

//	fun logout() {
//
//		executeGet("api/auth/logout?uid=$uid") { resp ->
//			resp.assert()
//		}
//	}
}