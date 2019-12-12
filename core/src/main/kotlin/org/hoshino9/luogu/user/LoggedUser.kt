package org.hoshino9.luogu.user

import com.google.gson.JsonObject
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.utils.assert
import org.hoshino9.luogu.utils.executePost
import org.hoshino9.luogu.utils.params
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
	fun doFollow(user: IBaseUser, isFollow: Boolean = true): Boolean {
		return JsonObject().apply {
			addProperty("uid", user.uid)
			addProperty("relationship", if (isFollow) 1 else 0)
		}.params().let { param ->
			luogu.executePost("fe/api/user/updateRelationShip", param, referer("user/$uid#following")) {
				it.assert()

				true
			}
		}
	}
}

open class LoggedUserPage(uid: Int, val luogu: LuoGu) : UserPage(uid, luogu.client) {
	override val user: LoggedUser
		get() = LoggedUser(super.user.source, luogu)
}