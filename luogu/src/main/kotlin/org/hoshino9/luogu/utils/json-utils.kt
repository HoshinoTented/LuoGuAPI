package org.hoshino9.luogu.utils

import org.json.JSONObject
import kotlin.reflect.KProperty

// Json
@Suppress("UNCHECKED_CAST")
class JsonDelegate(val obj : JSONObject) {
	inline operator fun <reified T> getValue(thisRef : Any?, property : KProperty<*>) : T {
		return (if (obj.has(property.name)) obj[property.name] else null) as T
	}
}

val JSONObject.delegate : JsonDelegate
	get() {
		return JsonDelegate(this)
	}

inline fun <reified T> json(content : String, init : JSONObject.() -> T) : T {
	return json(content).run(init)
}

fun json(content : String) : JSONObject {
	return JSONObject(content)
}