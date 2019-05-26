package org.hoshino9.luogu.user

import okhttp3.MediaType
import okhttp3.MultipartBody
import okhttp3.RequestBody
import org.hoshino9.luogu.*
import org.hoshino9.luogu.problem.Solution
import org.hoshino9.luogu.record.Record
import org.hoshino9.luogu.results.SignedInStatus
import org.hoshino9.luogu.utils.*
import org.json.JSONObject
import java.io.File
import java.lang.IllegalStateException

/**
 * **你谷**用户类
 * 但仅限于已登录的用户
 * 未登录的用户请用 `String` 代替
 * 等到 Kotlin1.3 可以改用 `inline class`
 */
@Suppress("MemberVisibilityCanBePrivate", "unused", "UNUSED_PARAMETER")
open class LoggedUser(val luogu : LuoGu, uid : String) : User(uid, luogu.client) {
	companion object {
		/**
		 * 实例化一个 LoggedUser 对象
		 * @param luogu 已经登陆过的洛谷客户端
		 * @return 返回一个 LoggedUser 对象
		 *
		 * @see LuoGuUtils.getUserIdFromPage
		 */
		@JvmName("newInstance")
		operator fun invoke(luogu : LuoGu) : LoggedUser {
			if (luogu.isLogged.not()) throw IllegalArgumentException("no logged in")
			return LoggedUser(luogu, luogu.myuid)
		}
	}

	/**
	 * 获取签到状态信息
	 * @return 返回一个签到状态类
	 * @throws IllegalStateException 未签到时抛出
	 *
	 * @see SignedInStatus
	 */
	val signInStatus : SignedInStatus
		get() {
			val doc = luogu.page
			val node = doc.getElementsByClass("am-u-md-4 lg-punch am-text-center")?.first() ?: throw NoSuchElementException()
			return SignedInStatus(node.children())
		}

	override val spacePage : LoggedUserSpacePage by lazy { LoggedUserSpacePage(this) }

	/**
	 * **你谷**签到
	 */
	fun signIn() {
		return luogu.executeGet("index/ajax_punch") { resp ->
			resp.assert()
		}
	}

	/**
	 * 提交题解
	 * @param solution 题解对象
	 * @return 返回 Record 对象
	 *
	 * @see Solution
	 * @see Record
	 */
	@JvmOverloads
	fun postSolution(solution : Solution, verifyCode : String = "") : Record {
		return luogu.executePost("api/problem/submit/${solution.pid}",
				listOf(
						"code" to solution.code,
						"lang" to solution.language.value.toString(),
						"enableO2" to if (solution.enableO2) "1" else "0",
						"verify" to verifyCode
				).params(), referer("problemnew/show/${solution.pid}")
		) { resp ->
			resp.assert()
			val content = resp.strData

			json(content) {
				val status = this["status"] //optInt("status")
				val data = this["data"]

				if (status == 200) {
					data as JSONObject
					Record(data.get("rid").toString())
				} else throw IllegalAPIStatusCodeException(status, data)
			}
		}
	}

	/**
	 * 上传图片到**你谷**
	 * @param file 图片的 File 对象
	 * @throws IllegalAPIStatusCodeException 当 api 状态码不为 201 时抛出
	 * @throws IllegalStatusCodeException 当 请求状态码不为 200 时抛出
	 *
	 * @see File
	 */
	fun postPhoto(file : File) {
		luogu.executePost("app/upload", MultipartBody.Builder()
				.setType(MultipartBody.FORM)
				.addFormDataPart("picupload", file.name, RequestBody.create(MediaType.parse("application/octet-stream"), file))
				.build(),
				referer("app/upload")) { resp ->
			resp.assert()
			val content = resp.strData
			json (content) {
				if (this["code"] != 201) throw IllegalAPIStatusCodeException(this["code"])
			}
		}
	}

	/**
	 * 获取未阅读列表(就是右上角的新通知)
	 * @return 返回一个 Message 和 Notice 数量的Pair
	 */
	fun getUnread() : Pair<Int, Int> {
		return luogu.executeGet("space/ajax_getchatnum") { resp ->
			resp.assert()

			json(resp.strData) {
				getInt("code").let { code ->
					if (code != 200) throw IllegalAPIStatusCodeException(code, getString("message"))
					getJSONObject("more").let { more ->
						more.getInt("messagenum") to more.getInt("noticenum")
					}
				}
			}
		}
	}

	/**
	 * (un)?follow
	 */
	fun doFollow(user : User, isFollow : Boolean = true) : Boolean {
		return if (luogu.isLogged) {
			luogu.executeGet("space/show?uid=${user.uid}&myuid=${this.uid}&follow=${if (isFollow) 1 else 0}") {
				it.assert()

				true
			}
		} else false
	}
}