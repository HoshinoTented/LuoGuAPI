package org.hoshino9.luogu.photo

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.annotations.JsonAdapter
import com.google.gson.annotations.SerializedName
import org.hoshino9.luogu.user.BaseUser
import org.hoshino9.luogu.utils.Deserializable
import java.lang.reflect.Type

@JsonAdapter(PhotoImpl.Serializer::class)
interface Photo {
	val id: String
	val size: Int
	val date: String
	val user: BaseUser
	val url: String
}

data class PhotoImpl(
		override val id: String,
		override val size: Int,
		@SerializedName("uploadTime") override val date: String,
		@SerializedName("delegate") override val user: BaseUser,
		override val url: String
) : Photo {
	companion object Serializer : Deserializable<Photo>(Photo::class), JsonDeserializer<Photo> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): Photo {
			return context.deserialize(json, PhotoImpl::class.java)
		}
	}

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

