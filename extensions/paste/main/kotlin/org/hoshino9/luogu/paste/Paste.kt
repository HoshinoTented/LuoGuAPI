package org.hoshino9.luogu.paste

import com.google.gson.JsonObject
import com.google.gson.annotations.SerializedName
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.user.BaseUser
import org.hoshino9.luogu.user.IBaseUser
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.utils.*

interface IPaste {
	val id: String
	val user: IBaseUser
	val time: Long
	val data: String
	val public: Boolean
}

open class Paste(val source: JsonObject) : IPaste {
	protected val delegate = source.delegate

	override val id: String by delegate
	override val time: Long by delegate
	override val data: String by delegate
	override val public: Boolean by delegate
	override val user: IBaseUser
		get() = BaseUser(source["user"].asJsonObject)

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
