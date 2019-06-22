package org.hoshino9.luogu.user

import okhttp3.MediaType
import okhttp3.MultipartBody
import okhttp3.RequestBody
import org.hoshino9.luogu.*
import org.hoshino9.luogu.utils.*
import org.jsoup.nodes.Element
import org.jsoup.select.Elements
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
	data class SignedInStatus(val qian: Qian, val goods: List<Thing>, val bads: List<Thing>, val continuation: Int) {
		companion object Parser {
			private val regex = Regex("[宜忌]：([^ ]+) ([^ ]+)")

			/**
			 * 解析html代码并实例化一个 `SignedInStatus`
			 * 接受一个 `<div class="am-u-md-4 lg-punch am-text-center">...</div>` 的子元素集合
			 * @param page
			 * @return SignedInStatus
			 *
			 * @see Elements
			 * @see SignedInStatus
			 */
			@Throws(IllegalStateException::class)
			@JvmName("newInstance")
			operator fun invoke(page: Elements): SignedInStatus {
				val head = page.getOrNull(1) ?: throw NoSuchElementException("second element of $page")
				val body = page.lastOrNull() ?: throw NoSuchElementException("last element of $page")

				if (head.tagName() == "span") {
					val headText = head.text().run { substring(2 until length - 2) }
					val qian = Qian.values().firstOrNull { it.show == headText } ?: throw NoSuchElementException(headText)

					val goods = body.children().getOrNull(0) ?: throw NoSuchElementException("first element of $body")
					val bads = body.children().getOrNull(1) ?: throw NoSuchElementException("second element of $body")
					val bottom = body.children().getOrNull(2) ?: throw NoSuchElementException("third element of $body")

					return SignedInStatus(
							qian,
							goods.run(::parseThings),
							bads.run(::parseThings),
							bottom.children().firstOrNull { it.tagName() == "strong" }?.text()?.toInt() ?: throw NoSuchElementException("first element of tag name 'strong' in $bottom")
					)
				} else throw IllegalStateException("no signed in")
			}

			private fun parseThings(node: Element): List<Thing> {
				return regex.findAll(node.text()).map {
					Thing(it.groupValues[1], it.groupValues[2])
				}.toList()
			}
		}

		enum class Qian(val show: String) {
			VeryGood("大吉"),
			MidGood("中吉"),
			Good("小吉"),
			Middle("中平"),
			Bad("凶"),
			VeryBad("大凶"),
		}

		data class Thing(val name: String, val description: String)
	}

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