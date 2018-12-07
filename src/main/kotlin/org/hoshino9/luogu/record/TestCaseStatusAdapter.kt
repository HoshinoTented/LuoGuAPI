package org.hoshino9.luogu.record

import com.google.gson.*
import java.lang.reflect.Type

object TestCaseStatusAdapter : JsonSerializer<TestCase.Status>, JsonDeserializer<TestCase.Status> {
	override fun serialize(src : TestCase.Status?, typeOfSrc : Type?, context : JsonSerializationContext?) : JsonElement {
		return JsonPrimitive(src?.value ?: return JsonNull.INSTANCE)
	}

	override fun deserialize(json : JsonElement?, typeOfT : Type?, context : JsonDeserializationContext?) : TestCase.Status {
		return TestCase.Status.values().first { it.value == json !!.asInt }
	}
}