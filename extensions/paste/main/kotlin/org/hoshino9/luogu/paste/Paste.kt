package org.hoshino9.luogu.paste

import com.google.gson.JsonObject
import com.google.gson.annotations.SerializedName
import org.hoshino9.luogu.HTMLParseException
import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

interface Paste {
	open class PastePage(id: String, client: HttpClient = emptyClient) : AbstractLuoGuPage(client) {
		override val url: String = "$baseUrl/paste/$id"

		private val data get() = feInjection["currentData"].asJsonObject["paste"].asJsonObject

		fun newInstance(): Paste {
			return Factory(data).newInstance()
		}
	}

	open class Factory(val obj: JsonObject) {
		fun newInstance(): Paste {
			return gson.fromJson(obj, PasteData::class.java)
		}
	}

	val id: String
	val user: User
	val time: Long
	val data: String
	val isPublic: Boolean
}

data class PasteData(
		override val id: String,
		override val user: User,
		override val time: Long,
		override val data: String,
		@SerializedName("public") override val isPublic: Boolean
) : Paste {
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