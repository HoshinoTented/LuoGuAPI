package org.hoshino9.luogu.photo

import com.google.gson.JsonObject
import com.google.gson.annotations.SerializedName
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.utils.gson

interface IPhoto {
	val id: String
	val size: Int
	val date: String
	val user: User
	val url: String
}

open class Photo(
		override val id: String,
		override val size: Int,
		@SerializedName("uploadTime") override val date: String,
		@SerializedName("provider") override val user: User,
		override val url: String
) : IPhoto {
	companion object {
		operator fun invoke(obj: JsonObject): IPhoto {
			return gson.fromJson(obj, Photo::class.java)
		}
	}

	override fun equals(other: Any?): Boolean {
		if (this === other) return true
		if (other !is IPhoto) return false

		if (id != other.id) return false

		return true
	}

	override fun hashCode(): Int {
		return id.hashCode()
	}
}

