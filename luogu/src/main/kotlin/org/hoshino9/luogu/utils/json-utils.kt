package org.hoshino9.luogu.utils

import org.json.JSONObject

// Json
inline fun <T> json(content : String, init : JSONObject.() -> T) : T {
	return JSONObject(content).run(init)
}

fun json(content : String) : JSONObject {
	return JSONObject(content)
}