package org.hoshino9.luogu

import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.HttpGet
import org.apache.http.client.methods.HttpPost
import org.apache.http.message.BasicNameValuePair
import org.apache.http.util.EntityUtils
import org.json.JSONObject
import org.jsoup.Jsoup

@Suppress("MemberVisibilityCanBePrivate", "unused", "UNUSED_PARAMETER")
class LuoGuUser(val luogu : LuoGu, val userid : String) {
	companion object {
		/**
		 * 实例化一个 LuoGuUser 对象
		 * @param luogu 已经登陆过的洛谷客户端
		 * @return 返回一个 LuoGuUser 对象
		 *
		 * @see LuoGu.Companion.userId
		 */
		@JvmName("newInstance")
		operator fun invoke(luogu : LuoGu) : LuoGuUser {
			HttpGet(LuoGu.baseUrl).let(luogu.client::execute)!!.let { resp ->
				val statusCode = resp.statusLine.statusCode
				val content = EntityUtils.toString(resp.entity)

				if (statusCode == 200) {
					return LuoGuUser(luogu, Jsoup.parse(content).run(LuoGu.Companion::userId) ?: throw LuoGuException(luogu, exceptionMessage("get user id", statusCode, "null uid")))
				} else throw LuoGuException(luogu, exceptionMessage("get user id", statusCode, content))
			}
		}
	}

	/**
	 * **你谷**签到
	 * @return 返回一个签到结果
	 *
	 * @see LuoGuSignedInResult
	 */
	fun signIn() : LuoGuSignedInResult? {
		return HttpGet("${LuoGu.baseUrl}/index/ajax_punch").let { req ->
			luogu.client.execute(req) !!.let { resp ->
				val statusCode = resp.statusLine.statusCode
				val content : String = EntityUtils.toString(resp.entity)
				if (statusCode == 200) {
					JSONObject(content).run {
						val code : Int = getInt("code")
						val msg : String = getString("message")

						LuoGuSignedInResult(code, msg)
					}
				} else throw LuoGuUserException(this, exceptionMessage("sign in", statusCode, content))
			}
		}
	}

	/**
	 * 获取犇犇列表
	 * @param type 犇犇的类型
	 * @param page 页数, 默认为`1`
	 * @return 返回一个犇犇的列表
	 *
	 * @see BenBen
	 * @see BenBenType
	 */
	@JvmOverloads
	@Throws(LuoGuUserException::class)
	fun benben(type : BenBenType, page : Int = 1) : List<BenBen> {
		HttpGet("${LuoGu.baseUrl}/feed/${type.toString().toLowerCase()}?page=$page").let { req ->
			luogu.client.execute(req) !!.let { resp ->
				val statusCode = resp.statusLine.statusCode
				val content = EntityUtils.toString(resp.entity)
				if (resp.statusLine.statusCode == 200) {
					return LuoGu.benben(Jsoup.parse(content))
				} else throw LuoGuUserException(this, exceptionMessage("load benben", statusCode, content))
			}
		}
	}

	/**
	 * 剪切板
	 * @param code `markdown` 代码
	 * @param public 是否公开, 默认 **true**
	 * @return 返回剪切板的代码
	 */
	@JvmOverloads
	@Throws(LuoGuUserException::class, LuoGuException::class)
	fun paste(code : String, public : Boolean = true) : String {
		HttpPost("${LuoGu.baseUrl}/paste/post").apply {
			addHeader("x-csrf-token", luogu.csrfToken)
		}.let { req ->
			req.entity = UrlEncodedFormEntity(
					listOf(
							"content" to code,
							"verify" to "",
							"public" to if (public) "1" else "0"
					).map { (n, v) ->
						BasicNameValuePair(n, v)
					}
			)

			luogu.client.execute(req).let { resp ->
				val statusCode = resp.statusLine.statusCode
				val content = resp.entity.data
				if (statusCode == 200) {
					JSONObject(content).let {
						if (it.optInt("status") == 200) {
							return it.getString("data")
						} else {
							throw LuoGuUserException(this, exceptionMessage("paste code", it.getInt("status"), it.getString("data")))
						}
					}
				} else throw LuoGuUserException(this, exceptionMessage("paste code", statusCode, content))
			}
		}
	}

	fun deletePaste(pasteId : String) {
		HttpPost("${LuoGu.baseUrl}/paste/delete/$pasteId").apply {
			addHeader("x-csrf-token", luogu.csrfToken)
		}.let { req ->
			luogu.client.execute(req)
		}
	}

	override fun toString() : String {
		return userid
	}
}