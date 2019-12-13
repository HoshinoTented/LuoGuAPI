package org.hoshino9.luogu.user

import com.google.gson.JsonObject
import io.ktor.client.request.post
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.utils.referer

interface IBaseLoggedUser : IBaseUser {

}

interface ILoggedUser : IBaseLoggedUser, IUser {

}

/**
 * **你谷**用户类
 */
open class LoggedUser(source: JsonObject, val luogu: LuoGu) : User(source), ILoggedUser {
	/**
	 * (un)?follow
	 */
	fun doFollow(user: IBaseUser, isFollow: Boolean = true) {
		JsonObject().apply {
			addProperty("uid", user.uid)
			addProperty("relationship", if (isFollow) 1 else 0)
		}.let { param ->
			runBlocking {
				luogu.client.post<String>("$baseUrl/fe/api/user/updateRelationShip") {
					referer("user/$uid#following")
					body = param
				}
			}
		}
	}
}

open class LoggedUserPage(uid: Int, val luogu: LuoGu) : UserPage(uid, luogu.client) {
	override val user: LoggedUser
		get() = LoggedUser(super.user.source, luogu)
}