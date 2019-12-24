package org.hoshino9.luogu.photo

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.annotations.JsonAdapter
import com.google.gson.annotations.SerializedName
import org.hoshino9.luogu.user.IBaseUser
import org.hoshino9.luogu.utils.Deserializable
import java.lang.reflect.Type

@JsonAdapter(Photo.Serializer::class)
interface IPhoto {
	val id: String
	val size: Int
	val date: String
	val user: IBaseUser
	val url: String
}

data class Photo(
		override val id: String,
		override val size: Int,
		@SerializedName("uploadTime") override val date: String,
		@SerializedName("provider") override val user: IBaseUser,
		override val url: String
) : IPhoto {
	companion object Serializer : Deserializable<IPhoto>(IPhoto::class), JsonDeserializer<IPhoto> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): IPhoto {
			return context.deserialize(json, Photo::class.java)
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

