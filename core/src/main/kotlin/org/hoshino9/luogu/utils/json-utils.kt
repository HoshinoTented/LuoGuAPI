package org.hoshino9.luogu.utils

import com.google.gson.*
import kotlin.reflect.KClass
import kotlin.reflect.KProperty

class JsonDelegate(val original: JsonObject, val context: JsonDeserializationContext? = null) {
	/**
	 * @throws NoSuchElementException will throw a exception when the element is not exists
	 */
	@Suppress("IMPLICIT_CAST_TO_ANY")
	operator fun <T> getValue(thisRef: Any?, property: KProperty<*>): T {
		val obj: JsonElement = original[property.name] ?: throw NoSuchElementException(property.name)
		val type = property.returnType.classifier as KClass<*>

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

				else -> (context ?: throw IllegalArgumentException("Can not cast ${property.name} to $type"))
						.deserialize(obj, type.java)
			}) as T
		} catch (e: TypeCastException) {
			throw TypeCastException("${property.name}(null) cannot be cast to non-null type $type")
		}
	}
}

abstract class Deserializable<T : Any>(private val `class`: KClass<T>) {
	companion object {
		val gson = Gson()
	}

	@JvmName("fromJson")
	operator fun invoke(source: JsonElement): T {
		return gson.fromJson(source, `class`.java)
	}
}

val JsonObject.delegate: JsonDelegate get() = delegateWith(null)
fun JsonObject.delegateWith(context: JsonDeserializationContext?) = JsonDelegate(this, context)

fun JsonElement.ifNull(): JsonElement? {
	return takeIf { it !is JsonNull }
}

fun json(content: String): JsonObject {
	return content.parseJson().asJsonObject
}

fun String.parseJson(): JsonElement {
	return JsonParser().parse(this)
}