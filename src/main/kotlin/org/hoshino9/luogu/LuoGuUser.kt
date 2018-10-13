package org.hoshino9.luogu

import org.apache.http.client.methods.HttpGet
import org.apache.http.util.EntityUtils
import org.json.JSONObject

@Suppress("MemberVisibilityCanBePrivate", "unused", "UNUSED_PARAMETER")
class LuoGuUser(val luogu : LuoGu, val userid : String) {
	companion object {
		@JvmName("newInstance")
		operator fun invoke(luogu : LuoGu) : LuoGuUser {
			HttpGet(LuoGu.baseUrl).let(luogu.client::execute)!!.let { resp ->
				val statusCode = resp.statusLine.statusCode
				val content = EntityUtils.toString(resp.entity)

				if (statusCode == 200) {
					return LuoGuUser(luogu, TODO("get user id"))
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
						val msg : String = getString("string")

						LuoGuSignedInResult(code, msg)
					}
				} else throw LuoGuUserException(this, exceptionMessage("sign in", statusCode, content))
			}
		}
	}

	inline fun recodes(filter : (Record) -> Boolean) : List<Record> {
		TODO()
	}

	fun recodes(filter : Record.RecordInfo) : List<Record> {
		return recodes {
			it.info == filter
		}
	}

	fun benben(type : BenBenType, page : Int = 1) {
		HttpGet("$luogu/feed/${type.toString().toLowerCase()}?page=$page").let { req ->
			luogu.client.execute(req) !!.let { resp ->
				val statusCode = resp.statusLine.statusCode
				val content = EntityUtils.toString(resp.entity)
				if (resp.statusLine.statusCode == 200) {
					TODO("get and parse benben")
				} else throw LuoGuUserException(this, exceptionMessage("load benben", statusCode, content))
			}
		}
	}
}