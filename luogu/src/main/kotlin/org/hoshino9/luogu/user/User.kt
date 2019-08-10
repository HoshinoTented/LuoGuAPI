@file:Suppress("unused")

package org.hoshino9.luogu.user

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.annotations.JsonAdapter
import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import java.lang.reflect.Type

@JsonAdapter(User.Companion.Deserializer::class)
open class User(val uid: String) {
	companion object {
		object Deserializer : JsonDeserializer<User> {
			override fun deserialize(json: JsonElement, typeOfT: Type?, context: JsonDeserializationContext?): User {
				return User(json.asJsonObject["uid"].toString())
			}
		}

		@JvmName("newInstance")
		operator fun invoke(elem : Element) : User {
//			if (elem.children().size != 1) return HasBadgeUser(elem)

			return elem.child(0).attr("href").run(LuoGuUtils::userFromUrl)
		}
	}

	private val page : Document by lazy {
		emptyClient.executeGet("$baseUrl/space/show?uid=$uid") { resp ->
			resp.assert()
			Jsoup.parse(resp.strData)
		}
	}

	open val spacePage : UserSpacePage by lazy { UserSpacePage(this) }

	override fun equals(other : Any?) : Boolean {
		if (this === other) return true
		if (other !is User) return false

		if (uid != other.uid) return false

		return true
	}

	override fun hashCode() : Int {
		return uid.hashCode()
	}

	override fun toString() : String {
		return uid
	}
}
