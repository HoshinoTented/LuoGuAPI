package org.hoshino9.luogu

import org.apache.http.client.HttpClient
import org.apache.http.client.methods.HttpGet
import org.apache.http.client.methods.HttpPost
import org.apache.http.impl.client.HttpClients
import org.apache.http.util.EntityUtils
import org.json.JSONObject
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import java.io.OutputStream

@Suppress("MemberVisibilityCanBePrivate")
class LuoGu @JvmOverloads constructor(val client : HttpClient = HttpClients.createDefault()) {
	companion object {
		const val baseUrl = "https://www.luogu.org"

		/**
		 * 获取 uid
		 * @param document Document对象, 即**你谷**主站页面, 因为**你谷**某些奇怪的原因, 而没有开放API, 所以只能从网页中爬了
		 * @return 返回一个uid, **Nullable**
		 *
		 * @see Document
		 */
		fun userId(document : Document) : String? {
			return document.body().getElementsByTag("header")?.first()?.getElementsByAttribute("myuid")?.attr("myuid")
		}

		/**
		 * **TODO**
		 * 因为**你谷**不让别人爬评测记录, 所以就不能爬了
		 * 获取评测记录
		 * @param page Document对象, 即**你谷**主站页面
		 * @param filter 用于过滤的函数
		 * @return 返回一个评测记录列表
		 *
		 * @see Document
		 * @see Record
		 */
		inline fun records(page : Document, filter : (Record) -> Boolean = { true }) : List<Record> {
			TODO("""评测记录相关页面不欢迎一切爬虫行为。
我相信如果你正在制作爬虫，一定能够看到本段文字。
请勿再制作任何爬取评测记录的爬虫。""")
		}

		/**
		 * 解析benben
		 * @param page 犇犇的 `html` 代码
		 * @return 返回一个犇犇列表
		 *
		 * @see BenBen
		 */
		fun benben(page : Document) : List<BenBen> {
			TODO()
		}

		/**
		 * 一个奇怪的Token, 似乎十分重要, 大部分操作都需要这个
		 * @param page 任意一个**你谷**页面
		 * @return 返回 `csrf-token`, 若找不到则返回 **null**
		 */
		fun csrfToken(page : Document) : String? {
			return page.head().getElementsByTag("meta").firstOrNull { it?.attr("name") == "csrf-token" }?.attr("content")
		}
	}

	/**
	 * 一个奇怪的Token, 似乎十分重要, 大部分操作都需要这个
	 */
	@get:Throws(LuoGuException::class)
	val csrfToken : String get() {
		HttpGet(baseUrl).let { req ->
			client.execute(req)!!.let { resp ->
				return csrfToken(Jsoup.parse(EntityUtils.toString(resp.entity))) ?: throw LuoGuException(this, exceptionMessage("get csrf-token", resp.statusLine.statusCode, "No such csrf-token"))
			}
		}
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
				val content : ByteArray = EntityUtils.toByteArray(resp.entity)
				if (statusCode == 200) {
					output.write(content)
				} else throw LuoGuException(this, exceptionMessage("get verify code image", statusCode, String(content)))
			}
		}
	}

	/**
	 * 登录**你谷**
	 * @param account 账号
	 * @param password 密码
	 * @param verifyCode 验证码, 通过 LuoGu::verifyCode 获得
	 * @param action 接受一个 LuoGuLoginResult, 返回是否可以实例化一个 LuoGuUser
	 * @return 返回一个 LuoGuLoginResule 对象
	 *
	 * @see LuoGu.verifyCode
	 * @see LuoGuUser
	 * @see LuoGuLoginResult
	 */
	@Throws(LuoGuException::class)
	fun login(account : String, password : String, verifyCode : String, action : (LuoGuLoginResult) -> Boolean) : LuoGuUser? =
			if (action(login(account, password, verifyCode))) LuoGuUser(this) else null

	/**
	 * 登录**你谷**
	 * @param account 账号
	 * @param password 密码
	 * @param verifyCode 验证码, 通过 LuoGu::verifyCode 获得
	 * @return 返回一个 LuoGuLoginResule 对象
	 *
	 * @see LuoGu.verifyCode
	 * @see LuoGuLoginResult
	 */
	@Throws(LuoGuException::class)
	fun login(account : String, password : String, verifyCode : String) : LuoGuLoginResult {
		return HttpPost("$baseUrl/login/loginpage").apply {
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
					JSONObject(content).run {
						val code : Int = getInt("code")
						val msg : String = getString("message")
						val more : JSONObject? = optJSONObject("more")
						val goto : String? = more?.getString("goto")

						LuoGuLoginResult(code, msg, goto)
					}
				} else throw LuoGuException(this, exceptionMessage("login", statusCode, content))
			}
		}
	}
}