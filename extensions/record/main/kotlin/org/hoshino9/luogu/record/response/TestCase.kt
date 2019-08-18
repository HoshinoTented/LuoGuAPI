package org.hoshino9.luogu.record.response

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import java.lang.reflect.Type

data class TestCase(
		val status: Response.Status,
		val message: String,
		val memory: Int,
		val score: Int,
		val time: Int
)