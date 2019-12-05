package org.hoshino9.luogu.paste

import com.google.gson.JsonObject
import com.google.gson.annotations.SerializedName
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.utils.*

interface IPaste {
	val id: String
	val user: User
	val time: Long
	val data: String
	val isPublic: Boolean
}

open class Paste(
		override val id: String,
		override val user: User,
		override val time: Long,
		override val data: String,
		@SerializedName("public") override val isPublic: Boolean
) : IPaste {
	companion object {
		operator fun invoke(obj: JsonObject): IPaste {
			return gson.fromJson(obj, Paste::class.java)
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

	fun newInstance(): IPaste {
		return Paste(data)
	}
}
