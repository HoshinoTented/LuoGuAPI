@file:JvmName("PasteUtils")

package org.hoshino9.luogu.paste

import com.google.gson.JsonObject
import com.google.gson.JsonParser
import org.hoshino9.luogu.IllegalStatusCodeException
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
fun LoggedUser.newPaste(code: String, public: Boolean = true): String {
	return luogu.executePost("paste/new", JsonObject().apply {
		addProperty("data", code)
		addProperty("public", public)
	}.params(), referer("paste")) { resp ->
		resp.assert()

		val content = resp.strData
		println(content)
		json(content).let {
			it["id"].asString
		}
	}
}

fun LoggedUser.deletePaste(id: String) {
	luogu.executePost("paste/delete/$id", headers = referer("paste/$id")) { resp ->
		resp.assert()
	}
}

fun LoggedUser.editPaste(id: String, data: String, public: Boolean) {
	luogu.executePost("paste/edit/$id", JsonObject().apply {
		addProperty("data", data)
		addProperty("id", id)
		addProperty("public", public)
	}.params(), referer("paste/$id")) { resp ->
		resp.assert()
	}
}

fun LoggedUser.pasteList(page: Int = 1): PasteList {
	return PasteList(page, luogu.client)
}