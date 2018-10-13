package org.hoshino9.luogu

import org.apache.http.client.HttpClient
import org.apache.http.client.methods.HttpGet
import org.apache.http.client.methods.HttpPost
import org.apache.http.impl.client.HttpClients
import org.apache.http.util.EntityUtils
import org.json.JSONObject
import java.io.OutputStream

@Suppress("MemberVisibilityCanBePrivate")
class LuoGu(val client : HttpClient = HttpClients.createDefault()) {
	companion object {
		const val baseUrl = "https://www.luogu.org"
	}

	/**
	 * 获取验证码
	 * @param output 输出流, 将会把验证码**图片**输出到这个流里
	 * @throws LuoGuException
	 */
	@Throws(LuoGuException::class)
	fun verifyCode(output : OutputStream) {
		HttpGet("$baseUrl/download/captcha").let { req ->
			client.execute(req)!!.let { resp ->
				val statusCode = resp.statusLine.statusCode
				val content : String = EntityUtils.toString(resp.entity)
				if (statusCode == 200) {
					resp.entity.writeTo(output)
				} else throw LuoGuException(this, exceptionMessage("get verify code image", statusCode, content))
			}
		}
	}

	/**
	 * 登录**你谷**
	 * @param account 账号
	 * @param password 密码
	 * @param verifyCode 验证码, 通过 LuoGu::verifyCode 获得
	 * @param action 登录时需要做的东西, 接受一个 LuoGuLoginResult
	 * @return 返回一个 LuoGuUser 对象
	 *
	 * @see LuoGu.verifyCode
	 * @see LuoGuUser
	 * @see LuoGuLoginResult
	 */
	inline fun login(account : String, password : String, verifyCode : String, action : (LuoGuLoginResult) -> Unit = {}) : LuoGuUser {
		HttpPost("$baseUrl/login/loginpage").apply {
			val cookie = 0
			val redirect = ""
			val twoFactor = "undefined"

			entity = mapOf(
					"username" to account,
					"password" to password,
					"cookie" to cookie.toString(),
					"redirect" to redirect,
					"two_factor" to twoFactor,
					"verify" to verifyCode
			).entity()
		}.let { req ->
			client.execute(req)!!.let { resp ->
				val statusCode = resp.statusLine.statusCode
				val content : String = EntityUtils.toString(resp.entity)
				if (resp.statusLine.statusCode == 200) {
					JSONObject(content).apply {
						val code : Int = getInt("code")
						val msg : String = getString("string")
						val more : JSONObject = getJSONObject("more")
						val goto : String = more.getString("goto")

						LuoGuLoginResult(code, msg, goto).run(action)
					}
				} else throw LuoGuException(this, exceptionMessage("login", statusCode, content))
			}
		}

		return LuoGuUser(this)
	}
}