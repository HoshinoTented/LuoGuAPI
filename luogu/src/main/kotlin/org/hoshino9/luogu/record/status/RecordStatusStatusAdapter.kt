package org.hoshino9.luogu.record.status

import com.google.gson.*
import java.lang.reflect.Type

object RecordStatusStatusAdapter : JsonSerializer<RecordStatus.Status>, JsonDeserializer<RecordStatus.Status> {
	override fun serialize(src : RecordStatus.Status?, typeOfSrc : Type?, context : JsonSerializationContext?) : JsonElement {
		return JsonPrimitive(src?.value ?: return JsonNull.INSTANCE)
	}

	override fun deserialize(json : JsonElement, typeOfT : Type?, context : JsonDeserializationContext?) : RecordStatus.Status {
		return json.run {
			RecordStatus.Status.values().first { it.value == asInt }
		}
	}
}