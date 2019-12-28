package org.hoshino9.luogu.paste

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.annotations.JsonAdapter
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.user.BaseUser
import org.hoshino9.luogu.utils.Deserializable
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.emptyClient
import java.lang.reflect.Type

@JsonAdapter(PasteImpl.Serializer::class)
interface Paste {
	val id: String
	val user: BaseUser
	val time: Long
	val data: String
	val public: Boolean
}

data class PasteImpl(override val id: String, override val user: BaseUser, override val time: Long, override val data: String, override val public: Boolean) : Paste {
	companion object Serializer : Deserializable<Paste>(Paste::class), JsonDeserializer<Paste> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): Paste {
			return context.deserialize(json, PasteImpl::class.java)
		}
	}

	override fun equals(other: Any?): Boolean {
		if (this === other) return true
		if (other !is Paste) return false

		if (id != other.id) return false

		return true
	}

	override fun hashCode(): Int {
		return id.hashCode()
	}
}

open class PastePage(id: String, client: HttpClient = emptyClient) : AbstractLuoGuPage(client) {
	override val url: String = "$baseUrl/paste/$id"

	private val data get() = feInjection["currentData"].asJsonObject["paste"].asJsonObject

	val paste: Paste get() = PasteImpl(data)
}
