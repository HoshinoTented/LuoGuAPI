package org.hoshino9.luogu.utils

import com.google.gson.*
import kotlin.reflect.KProperty

// Json
class JsonDelegate(val original: JsonObject, val context: JsonDeserializationContext? = null) {
	/**
	 * @throws NoSuchElementException will throw a exception when the element is not exists
	 */
	@Suppress("IMPLICIT_CAST_TO_ANY")
	inline operator fun <reified T> getValue(thisRef: Any?, property: KProperty<*>): T {
		val obj: JsonElement = original[property.name] ?: throw NoSuchElementException(property.name)

		try {
			return (if (obj is JsonNull) null else when (T::class) {
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

				else -> context?.deserialize(obj, T::class.java) ?: throw IllegalArgumentException("Can not cast ${property.name} to ${T::class}")
			}) as T
		} catch (e: TypeCastException) {
			throw TypeCastException("${property.name}(null) cannot be cast to non-null type ${T::class}")
		}
	}
}

val JsonObject.delegate: JsonDelegate get() = delegateWith(null)
fun JsonObject.delegateWith(context: JsonDeserializationContext?) = JsonDelegate(this, context)

fun JsonElement.ifNull(): JsonElement? {
	return takeIf { it !is JsonNull }
}

inline fun <reified T> json(content: String, init: JsonObject.() -> T): T {
	return json(content).run(init)
}

fun json(content: String): JsonObject {
	return JsonParser().parse(content).asJsonObject
}

// Gson
val gson = Gson()