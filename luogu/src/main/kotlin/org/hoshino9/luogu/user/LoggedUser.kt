package org.hoshino9.luogu.user

import okhttp3.MediaType
import okhttp3.MultipartBody
import okhttp3.OkHttpClient
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
open class LoggedUser(val luogu: LuoGu, uid: Int) : User(uid) {
	companion object {
		/**
		 * 实例化一个 LoggedUser 对象
		 * @param luogu 已经登陆过的洛谷客户端
		 * @return 返回一个 LoggedUser 对象
		 */
		@JvmName("newInstance")
		operator fun invoke(luogu : LuoGu) : LoggedUser {
			require(! luogu.isLogged.not()) { "no logged in" }
			return LoggedUser(luogu, luogu.uid.toInt())
		}
	}

	override val client: OkHttpClient
		get() = luogu.client

	/**
	 * (un)?follow
	 *
	 * TODO may be deprecated
	 */
	fun doFollow(user : User, isFollow : Boolean = true) : Boolean {
		return if (luogu.isLogged) {
			luogu.executeGet("space/show?uid=${user.uid}&uid=${this.uid}&follow=${if (isFollow) 1 else 0}") {
				it.assert()

				true
			}
		} else false
	}
}