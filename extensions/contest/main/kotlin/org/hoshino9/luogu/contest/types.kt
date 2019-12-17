package org.hoshino9.luogu.contest

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.annotations.JsonAdapter
import java.lang.reflect.Type

internal object RuleTypeSerializer : JsonDeserializer<RuleType> {
	override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): RuleType {
		return RuleType.values()[json.asInt]
	}
}

internal object VisibilityTypeSerializer : JsonDeserializer<VisibilityType> {
	override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): VisibilityType {
		return VisibilityType.values()[json.asInt]
	}
}

@JsonAdapter(RuleTypeSerializer::class)
enum class RuleType {
	None,
	OI,
	ACM,
	LD,            // 乐多
	IOI,
	CF,
	DS            // 夺时
}

@JsonAdapter(VisibilityTypeSerializer::class)
enum class VisibilityType {
	Banned,
	Official,
	OrganizationPublic,
	OrganizationInternal,
	PersonalPublic,
	PersonalInvite,
	OrganizationInvite,
	OrganizationPublicVerifying,
	PersonalPublicVerifying
}