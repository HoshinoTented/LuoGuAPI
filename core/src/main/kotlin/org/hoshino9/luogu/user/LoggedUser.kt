package org.hoshino9.luogu.user

import com.google.gson.JsonObject
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
 */
@Suppress("MemberVisibilityCanBePrivate", "unused", "UNUSED_PARAMETER")
open class LoggedUser(val luogu: LuoGu, uid: Int) : User(uid, luogu.client) {
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

	/**
	 * (un)?follow
	 */
	fun doFollow(user : User, isFollow : Boolean = true) : Boolean {
		return if (luogu.isLogged) {
			JsonObject().apply {
				addProperty("uid", user.uid)
				addProperty("relationship", if (isFollow) 1 else 0)
			}.params().let { param ->
				luogu.executePost("fe/api/user/updateRelationShip", param, referer("user/$uid#following")) {
					it.assert()

					true
				}
			}
		} else false
	}
}