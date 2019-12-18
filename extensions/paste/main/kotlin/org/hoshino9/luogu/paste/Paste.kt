package org.hoshino9.luogu.paste

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.JsonObject
import com.google.gson.annotations.JsonAdapter
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.user.BaseUser
import org.hoshino9.luogu.user.IBaseUser
import org.hoshino9.luogu.utils.Deserializable
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.delegate
import org.hoshino9.luogu.utils.emptyClient
import java.lang.reflect.Type

@JsonAdapter(Paste.Serializer::class)
interface IPaste {
	val id: String
	val user: IBaseUser
	val time: Long
	val data: String
	val public: Boolean
}

data class Paste(override val id: String, override val user: IBaseUser, override val time: Long, override val data: String, override val public: Boolean) : IPaste {
	companion object Serializer : Deserializable<IPaste>(IPaste::class), JsonDeserializer<IPaste> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): IPaste {
			return context.deserialize(json, Paste::class.java)
		}
	}

	override fun equals(other: Any?): Boolean {
		if (this === other) return true
		if (other !is IPaste) return false

		if (id != other.id) return false

		return true
	}

	override fun hashCode(): Int {
		return id.hashCode()
	}

	override fun toString(): String {
		return id
	}
}

open class PastePage(id: String, client: HttpClient = emptyClient) : AbstractLuoGuPage(client) {
	override val url: String = "$baseUrl/paste/$id"

	private val data get() = feInjection["currentData"].asJsonObject["paste"].asJsonObject

	val paste: IPaste get() = Paste(data)
}
