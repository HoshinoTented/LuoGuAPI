package org.hoshino9.luogu.photo

import com.google.gson.JsonObject
import com.google.gson.annotations.SerializedName
import org.hoshino9.luogu.HTMLParseException
import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.MatchException
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.utils.gson
import org.jsoup.nodes.Element

interface Photo {
	open class Factory(val obj: JsonObject) {
		fun newInstance(): Photo {
			return gson.fromJson(obj, PhotoData::class.java)
		}
	}

	val id: String
	val size: Int
	val date: String
	val user: User
	val url: String
}

data class PhotoData(
		override val id: String,
		override val size: Int,
		@SerializedName("uploadTime") override val date: String,
		@SerializedName("provider") override val user: User,
		override val url: String
) : Photo {
	override fun equals(other: Any?): Boolean {
		if (this === other) return true
		if (other !is Photo) return false

		if (id != other.id) return false

		return true
	}

	override fun hashCode(): Int {
		return id.hashCode()
	}
}

