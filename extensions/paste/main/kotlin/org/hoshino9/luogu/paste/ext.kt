@file:JvmName("PasteUtils")

package org.hoshino9.luogu.paste

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
fun LoggedUser.postPaste(code: String, public: Boolean = true, verifyCode: String = ""): String {
	return luogu.executePost("paste/post", listOf(
			"content" to code,
			"verify" to verifyCode,
			"public" to if (public) "1" else "0"
	).params(), referer("paste")) { resp ->
		resp.assert()

		val content = resp.strData
		json(content).delegate.let {
			val status: Int by it
			val data: String? by it

			if (status != 200) throw IllegalStatusCodeException(status, data ?: "")
			data !!
		}
	}
}

fun LoggedUser.deletePaste(id: String) {
	luogu.executePost("paste/delete/$id", headers = referer("paste/$id")) { resp ->
		resp.assert()
	}
}

fun LoggedUser.pasteList(): PasteList {
	return PasteList(luogu.client)
}