package org.hoshino9.luogu.record.status

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import java.lang.reflect.Type

object RecordStatusAdapter : JsonDeserializer<RecordStatus> {
	override fun deserialize(json : JsonElement, typeOfT : Type?, context : JsonDeserializationContext?) : RecordStatus {
		return RecordStatus(json)
	}
}