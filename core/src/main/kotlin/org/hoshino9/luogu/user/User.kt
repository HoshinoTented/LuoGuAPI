@file:Suppress("unused")

package org.hoshino9.luogu.user

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.JsonObject
import com.google.gson.annotations.JsonAdapter
import okhttp3.OkHttpClient
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.utils.*
import java.lang.reflect.Type

typealias ProblemID = String
typealias UID = Int

interface IBaseUser {
	val uid: Int
	val name: String
	val color: String
	val badge: String?
	val slogan: String
	val ccfLevel: Int
	val isAdmin: Boolean
	val isBanned: Boolean
}

open class BaseUser(val source: JsonObject) : IBaseUser {
	protected val delegate = source.delegate

	override val uid: Int by delegate
	override val name: String by delegate
	override val color: String by delegate
	override val badge: String? by delegate
	override val slogan: String by delegate
	override val ccfLevel: Int by delegate
	override val isAdmin: Boolean by delegate
	override val isBanned: Boolean by delegate

	override fun equals(other: Any?): Boolean {
		return (other as? BaseUser)?.uid == uid
	}

	override fun hashCode(): Int {
		return uid.hashCode()
	}

	override fun toString(): String {
		return uid.toString()
	}
}

interface IUser : IBaseUser {
	val ranking: Int
	val introduction: String
}

open class User(source: JsonObject) : BaseUser(source), IUser {
	override val uid: Int by delegate
	override val badge: String? by delegate
	override val ccfLevel: Int by delegate
	override val isBanned: Boolean by delegate
	override val slogan: String by delegate
	override val introduction: String by delegate
	override val isAdmin: Boolean by delegate
	override val name: String by delegate
	override val ranking: Int by delegate
	override val color: String by delegate
}

open class UserPage(val uid: Int, client: HttpClient = emptyClient) : AbstractLuoGuPage(client) {
	override val url: String get() = "$baseUrl/user/$uid"

	protected val data = currentData

	open val user: User
		get() {
			return User(data["user"].asJsonObject)
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

	fun followers(page: Int = 1): FollowList = FollowList(user, page, FollowList.Type.Followers)
	fun followings(page: Int = 1): FollowList = FollowList(user, page, FollowList.Type.Followings)
}