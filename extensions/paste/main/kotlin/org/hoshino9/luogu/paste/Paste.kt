package org.hoshino9.luogu.paste

import org.hoshino9.luogu.HTMLParseException
import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

data class Paste(
		val id: String,
		val user: User,
		val url: String,
		val date: String,
		val source: String,
		val isPublic: Boolean
) {
	open class DefaultPaste(open val id: String, val client: HttpClient = defaultClient) {
		protected val body: Element get() = elem.getElementsByClass("lg-article").first() ?: throw HTMLParseException(elem)

		val elem: Element
			get() {
				return client.executeGet(url) { resp ->
					resp.assert()
					val content = resp.strData

					Jsoup.parse(content)
				}
			}

		open val url: String get() = "${LuoGuUtils.baseUrl}/paste/$id"

		open val user: User
			get() = body.child(0).child(0).attr("href").run(LuoGuUtils::userFromUrl)

		open val date: String
			get() = body.child(0).textNodes()[1].text().trim().substring(6)

		open val isPublic: Boolean get() = body.child(1).text() == "公开"

		open val source: String get() = body.children().last().text()

		fun newInstance(): Paste {
			return Paste(id, user, url, date, source, isPublic)
		}
	}

	override fun equals(other: Any?): Boolean {
		if (this === other) return true
		if (other !is Paste) return false

		if (id != other.id) return false

		return true
	}

	override fun hashCode(): Int {
		return id.hashCode()
	}
}