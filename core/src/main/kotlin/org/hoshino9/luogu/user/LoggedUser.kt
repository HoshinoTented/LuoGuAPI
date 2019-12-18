package org.hoshino9.luogu.user

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.JsonObject
import com.google.gson.annotations.JsonAdapter
import io.ktor.client.call.receive
import io.ktor.client.request.post
import io.ktor.client.response.HttpResponse
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.utils.*
import java.lang.reflect.Type

interface IBaseLoggedUser : IBaseUser {

}

@JsonAdapter(LoggedUser.Serializer::class)
interface ILoggedUser : IBaseLoggedUser, IUser {

}

/**
 * **你谷**用户类
 */
data class LoggedUser(val user: IUser) : IUser by user, ILoggedUser {
	companion object Serializer : JsonDeserializer<ILoggedUser> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): ILoggedUser {
			val user = context.deserialize<IUser>(json, IUser::class.java)

			return LoggedUser(user)
		}

		operator fun invoke(json: JsonElement): ILoggedUser {
			return gson.fromJson(json, ILoggedUser::class.java)
		}
	}

	override fun equals(other: Any?): Boolean {
		return user == other
	}

	override fun hashCode(): Int {
		return user.hashCode()
	}
}

open class LoggedUserPage(uid: Int, val luogu: LuoGu) : UserPage(uid, luogu.client) {
	override val user: ILoggedUser
		get() = LoggedUser(userObj)
}

/**
 * 关注用户
 *
 * @param userId 目标用户 ID
 * @param isFollow true 为关注，false 为取关
 */
suspend fun LuoGu.doFollow(userId: Int, isFollow: Boolean = true) {
	JsonObject().apply {
		addProperty("uid", userId)
		addProperty("relationship", if (isFollow) 1 else 0)
	}.let { param ->
		apiPost("fe/api/user/updateRelationShip") {
			referer("user/$uid#following")
			body = param.asParams
		}.receive<String>()
	}
}