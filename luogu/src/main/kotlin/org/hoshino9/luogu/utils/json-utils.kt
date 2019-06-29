package org.hoshino9.luogu.utils

import com.google.gson.*
import kotlin.reflect.KProperty

// Json
class JsonDelegate(val original: JsonObject) {
	@Suppress("IMPLICIT_CAST_TO_ANY")
	inline operator fun <reified T> getValue(thisRef: Any?, property: KProperty<*>): T {
		val obj: JsonElement? = original[property.name]?.takeIf { it != JsonNull.INSTANCE }

		return (if (obj == null) null else when (T::class) {
			JsonObject::class -> obj.asJsonObject
			JsonArray::class -> obj.asJsonArray
			JsonPrimitive::class -> obj.asJsonPrimitive

			String::class -> obj.asString
			Boolean::class -> obj.asBoolean
			Char::class -> obj.asCharacter

			Byte::class -> obj.asByte
			Short::class -> obj.asShort
			Int::class -> obj.asInt
			Long::class -> obj.asLong
			Float::class -> obj.asFloat
			Double::class -> obj.asDouble

			else -> throw IllegalArgumentException("Not in bounds: ${T::class}")
		}) as T
	}
}

val JsonObject.delegate: JsonDelegate
	get() {
		return JsonDelegate(this)
	}

inline fun <reified T> json(content: String, init: JsonObject.() -> T): T {
	return json(content).run(init)
}

fun json(content: String): JsonObject {
	return JsonParser().parse(content).asJsonObject
}

// Gson
val gson = Gson()