@file:Suppress("unused")

package org.hoshino9.luogu.user

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.annotations.JsonAdapter
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.utils.*
import java.lang.reflect.Type

typealias ProblemID = String

interface IUser {
	val uid: Int
	val ranking: Int
	val name: String
	val color: String
	val isAdmin: Boolean
	val background: String
	val introduction: String
	val passedProblems: List<ProblemID>
	val submittedProblems: List<ProblemID>
}

@JsonAdapter(User.Companion.Deserializer::class)
open class User(override val uid: Int) : AbstractLuoGuPage(), IUser {
	companion object {
		object Deserializer : JsonDeserializer<User> {
			override fun deserialize(json: JsonElement, typeOfT: Type?, context: JsonDeserializationContext?): User {
				return User(json.asJsonObject["uid"].toString().toInt())
			}
		}
	}

	private val data = feInjection["currentData"].asJsonObject
	private val userData = data["user"].asJsonObject
	private val delegate = userData.delegate

	override val background: String by delegate
	override val introduction: String by delegate
	override val isAdmin: Boolean by delegate
	override val name: String by delegate
	override val ranking: Int by delegate
	override val color: String by delegate

	private fun problemList(attr: String): List<ProblemID> {
		return data[attr].asJsonArray.map {
			it.asJsonObject.let {
				it["pid"].asString
			}
		}
	}

	override val passedProblems: List<ProblemID> get() = problemList("passedProblems")
	override val submittedProblems: List<ProblemID> get() = problemList("submittedProblems")

	override val url: String
		get() = "$baseUrl/user/$uid"

	override fun equals(other: Any?): Boolean {
		if (this === other) return true
		if (other !is User) return false

		if (uid != other.uid) return false

		return true
	}

	override fun hashCode(): Int {
		return uid.hashCode()
	}

	override fun toString(): String {
		return uid.toString()
	}
}
