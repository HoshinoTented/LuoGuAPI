@file:JvmName("PasteUtils")

package org.hoshino9.luogu.paste

import org.hoshino9.luogu.IllegalAPIStatusCodeException
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
fun LoggedUser.postPaste(code: String, public: Boolean = true, verifyCode: String = ""): Paste {
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

			if (status != 200) throw IllegalAPIStatusCodeException(status, data ?: "")
			Paste.Factory(data !!, luogu.client).newInstance()
		}
	}
}

fun LoggedUser.deletePaste(paste: Paste) {
	luogu.executePost("paste/delete/${paste.id}", headers = referer("paste/${paste.id}")) { resp ->
		resp.assert()
	}
}

@JvmOverloads
fun LoggedUser.pasteList(page: Int = 1): List<Paste> {
	val regex = Regex("""https://www\.luogu\.org/paste/(\w+)""")
	luogu.executeGet("paste?page=$page") { resp ->
		resp.assert()
		val content = resp.strData

		return Jsoup.parse(content).toString().run { regex.findAll(this) }.map {
			Paste.Factory(it.groupValues[1], luogu.client).newInstance()
		}.toList()
	}
}