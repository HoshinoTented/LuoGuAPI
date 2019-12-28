package org.hoshino9.luogu.team

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.annotations.JsonAdapter
import org.hoshino9.luogu.utils.Deserializable
import java.lang.reflect.Type

@JsonAdapter(BaseTeamImpl.Serializer::class)
interface BaseTeam {
	val id: Int
	val name: String
}

data class BaseTeamImpl(override val id: Int, override val name: String) : BaseTeam {
	companion object Serializer : Deserializable<BaseTeam>(BaseTeam::class), JsonDeserializer<BaseTeam> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): BaseTeam {
			return context.deserialize(json, BaseTeamImpl::class.java)
		}
	}
}