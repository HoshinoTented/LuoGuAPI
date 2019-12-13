@file:JvmName("PasteUtils")

package org.hoshino9.luogu.paste

import com.google.gson.JsonObject
import com.google.gson.JsonParser
import io.ktor.client.call.receive
import org.hoshino9.luogu.IllegalStatusCodeException
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.user.LoggedUser
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup


/**
 * 剪切板
 * @param code `markdown` 代码
 * @param public 是否公开, 默认 **true**
 * @return 返回剪切板的代码
 */
@JvmOverloads
suspend fun LoggedUser.newPaste(code: String, public: Boolean = true): String {
	val json = JsonObject().apply {
		addProperty("data", code)
		addProperty("public", public)
	}.params

	return luogu.apiPost("paste/new") {
		referer("paste")
		body = json
	}.receive<String>().run(::json)["id"].asString
}

suspend fun LoggedUser.deletePaste(id: String) {
	luogu.apiPost("paste/delete/$id") {
		referer("paste/$id")
	}.receive<String>()
}

suspend fun LoggedUser.editPaste(id: String, data: String, public: Boolean) {
	val json = JsonObject().apply {
		addProperty("data", data)
		addProperty("id", id)
		addProperty("public", public)
	}.params

	luogu.apiPost("paste/edit/$id") {
		referer("paste/$id")
	}.receive<String>()
}

fun LoggedUser.pasteList(page: Int = 1): PasteList {
	return PasteList(page, luogu.client)
}