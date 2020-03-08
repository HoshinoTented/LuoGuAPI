package org.hoshino9.luogu.user

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.JsonObject
import com.google.gson.annotations.JsonAdapter
import io.ktor.client.call.receive
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.utils.*
import java.lang.reflect.Type

interface BaseLoggedUser : BaseUser {
	companion object;

}

@JsonAdapter(LoggedUserImpl.Serializer::class)
interface LoggedUser : BaseLoggedUser, User {
	companion object;

}

/**
 * **你谷**用户类
 */
data class LoggedUserImpl(val user: User) : User by user, LoggedUser {
	companion object Serializer : Deserializable<LoggedUser>(LoggedUser::class), JsonDeserializer<LoggedUser> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): LoggedUser {
			val user = context.deserialize<User>(json, User::class.java)

			return LoggedUserImpl(user)
		}
	}

	override fun equals(other: Any?): Boolean {
		return (other as? LoggedUserImpl)?.user == user
	}

	override fun hashCode(): Int {
		return user.hashCode()
	}
}

open class LoggedUserPage(uid: Int, val luogu: LuoGu) : UserPage(uid, luogu.client) {
	override val user: LoggedUser by lazy {
		LoggedUserImpl(userObj)
	}
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

suspend fun LuoGu.follow(userId: Int) = doFollow(userId, true)
suspend fun LuoGu.unfollow(userId: Int) = doFollow(userId, false)