@file:Suppress("unused")

package org.hoshino9.luogu.user

import com.google.gson.*
import com.google.gson.annotations.JsonAdapter
import okhttp3.OkHttpClient
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.utils.*
import java.lang.reflect.Type

typealias ProblemID = String
typealias UID = Int

@JsonAdapter(BaseUser.Serializer::class)
interface IBaseUser {
	/**
	 * 用户 id
	 */
	val uid: Int

	/**
	 * 用户名称
	 */
	val name: String

	/**
	 * 用户等级（颜色）
	 */
	val color: String

	/**
	 * 用户头衔
	 */
	val badge: String?

	/**
	 * 用户签名
	 */
	val slogan: String

	/**
	 * 用户 ccf 等级
	 */
	val ccfLevel: Int

	/**
	 * 是否管理员
	 */
	val isAdmin: Boolean

	/**
	 * 是否被封禁
	 */
	val isBanned: Boolean
}

data class BaseUser(override val uid: Int, override val name: String, override val color: String, override val badge: String?, override val slogan: String, override val ccfLevel: Int, override val isAdmin: Boolean, override val isBanned: Boolean) : IBaseUser {
	companion object Serializer : Deserializable<IBaseUser>(IBaseUser::class), JsonDeserializer<IBaseUser> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): IBaseUser {
			return context.deserialize(json, BaseUser::class.java)
		}
	}

	override fun equals(other: Any?): Boolean {
		return (other as? BaseUser)?.uid == uid
	}

	override fun hashCode(): Int {
		return uid.hashCode()
	}
}

@JsonAdapter(User.Serializer::class)
interface IUser : IBaseUser {
	val ranking: Int?
	val introduction: String
}

data class User(override val ranking: Int?, override val introduction: String, val baseUser: IBaseUser) : IBaseUser by baseUser, IUser {
	companion object Serializer : Deserializable<IUser>(IUser::class), JsonDeserializer<IUser> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): IUser {
			val source = json.asJsonObject

			val ranking: Int? = source["ranking"].ifNull()?.asInt
			val introduction: String = source["introduction"].asString
			val baseUser = context.deserialize<IBaseUser>(json, IBaseUser::class.java)

			return User(ranking, introduction, baseUser)
		}
	}

	override fun equals(other: Any?): Boolean {
		return baseUser == other
	}

	override fun hashCode(): Int {
		return baseUser.hashCode()
	}
}

open class UserPage(val uid: Int, client: HttpClient = emptyClient) : AbstractLuoGuPage(client) {
	override val url: String get() = "$baseUrl/user/$uid"

	protected val data = currentData
	protected val userObj: JsonObject = data["user"].asJsonObject

	open val user: IUser
		get() {
			return User(userObj)
		}

	private fun problemList(attr: String): List<ProblemID> {
		return data[attr].asJsonArray.map {
			it.asJsonObject.let {
				it["pid"].asString
			}
		}
	}

	val passedProblems: List<ProblemID> get() = problemList("passedProblems")
	val submittedProblems: List<ProblemID> get() = problemList("submittedProblems")

	fun followers(page: Int = 1): FollowList = FollowList(user, page, FollowList.Type.Followers, client)
	fun followings(page: Int = 1): FollowList = FollowList(user, page, FollowList.Type.Followings, client)
}