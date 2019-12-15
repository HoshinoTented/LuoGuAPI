@file:JvmName("PasteUtils")

package org.hoshino9.luogu.paste

import com.google.gson.JsonObject
import io.ktor.client.call.receive
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.utils.apiPost
import org.hoshino9.luogu.utils.asParams
import org.hoshino9.luogu.utils.json
import org.hoshino9.luogu.utils.referer


/**
 * 剪切板
 * @param code `markdown` 代码
 * @param public 是否公开, 默认 **true**
 * @return 返回剪切板的代码
 */
@JvmOverloads
suspend fun LuoGu.newPaste(code: String, public: Boolean = true): String {
	val json = JsonObject().apply {
		addProperty("data", code)
		addProperty("public", public)
	}.asParams

	return apiPost("paste/new") {
		referer("paste")
		body = json
	}.receive<String>().run(::json)["id"].asString
}

suspend fun LuoGu.deletePaste(id: String) {
	apiPost("paste/delete/$id") {
		referer("paste/$id")
	}.receive<String>()
}

suspend fun LuoGu.editPaste(id: String, data: String, public: Boolean) {
	val json = JsonObject().apply {
		addProperty("data", data)
		addProperty("id", id)
		addProperty("public", public)
	}.asParams

	apiPost("paste/edit/$id") {
		referer("paste/$id")
		body = json
	}.receive<String>()
}

fun LuoGu.pasteList(page: Int = 1): PasteList {
	return PasteList(page, client)
}