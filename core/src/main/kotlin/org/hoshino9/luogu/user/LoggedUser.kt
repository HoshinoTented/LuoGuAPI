package org.hoshino9.luogu.user

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.JsonObject
import com.google.gson.annotations.JsonAdapter
import io.ktor.client.request.post
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.utils.asParams
import org.hoshino9.luogu.utils.gson
import org.hoshino9.luogu.utils.referer
import java.lang.reflect.Type

interface IBaseLoggedUser : IBaseUser {

}

@JsonAdapter(LoggedUser.Serializer::class)
interface ILoggedUser : IBaseLoggedUser, IUser {

}

/**
 * **你谷**用户类
 */
open class LoggedUser(val user: IUser) : IUser by user, ILoggedUser {
	companion object Serializer : JsonDeserializer<ILoggedUser> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): ILoggedUser {
			val user = context.deserialize<IUser>(json, IUser::class.java)

			return LoggedUser(user)
		}

		operator fun invoke(json: JsonElement): ILoggedUser {
			return gson.fromJson(json, ILoggedUser::class.java)
		}
	}

	/**
	 * (un)?follow
	 */

}

open class LoggedUserPage(uid: Int, val luogu: LuoGu) : UserPage(uid, luogu.client) {
	override val user: ILoggedUser
		get() = LoggedUser(userObj)
}

suspend fun LuoGu.doFollow(user: IBaseUser, isFollow: Boolean = true) {
	JsonObject().apply {
		addProperty("uid", user.uid)
		addProperty("relationship", if (isFollow) 1 else 0)
	}.let { param ->
		client.post<String>("$baseUrl/fe/api/user/updateRelationShip") {
			referer("user/$uid#following")
			body = param.asParams
		}
	}
}