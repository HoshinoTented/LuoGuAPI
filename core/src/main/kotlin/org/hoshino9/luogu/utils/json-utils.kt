package org.hoshino9.luogu.utils

import com.google.gson.*
import kotlin.reflect.KClass
import kotlin.reflect.KProperty

class JsonDelegateProvider(val original: JsonObject, val context: JsonDeserializationContext?) {
	inline fun <reified T> provide(): JsonDelegate<T> {
		return provideDelegate(null, null)
	}

	inline operator fun <reified T> provideDelegate(thisRef: Any?, property: KProperty<*>?): JsonDelegate<T> {
		return JsonDelegate(original, T::class, context)
	}
}

class JsonDelegate<T>(val original: JsonObject, val type: KClass<*>, val context: JsonDeserializationContext? = null) {
	/**
	 * @throws NoSuchElementException will throw a exception when the element is not exists
	 */
	@Suppress("IMPLICIT_CAST_TO_ANY")
	operator fun getValue(thisRef: Any?, property: KProperty<*>): T {
		val obj: JsonElement = original[property.name] ?: throw NoSuchElementException(property.name)

		try {
			return (if (obj is JsonNull) null else when (type) {
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

				else -> context?.deserialize(obj, type.java) ?: throw IllegalArgumentException("Can not cast ${property.name} to $type")
			}) as T
		} catch (e: TypeCastException) {
			throw TypeCastException("${property.name}(null) cannot be cast to non-null type $type")
		}
	}
}

val JsonObject.provider: JsonDelegateProvider get() = providerWith(null)
fun JsonObject.providerWith(context: JsonDeserializationContext?) = JsonDelegateProvider(this, context)

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