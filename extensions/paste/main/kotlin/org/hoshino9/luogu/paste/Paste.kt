package org.hoshino9.luogu.paste

import org.hoshino9.luogu.HTMLParseException
import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

interface Paste {
	open class Factory(override val id: String, val client: HttpClient = defaultClient) : Paste {
		protected val body: Element get() = elem.getElementsByClass("lg-article").first() ?: throw HTMLParseException(elem)

		val elem: Element
			get() {
				return client.executeGet(url) { resp ->
					resp.assert()
					val content = resp.strData

					Jsoup.parse(content)
				}
			}

		override val url: String get() = "${LuoGuUtils.baseUrl}/paste/$id"

		override val user: User
			get() = body.child(0).child(0).attr("href").run(LuoGuUtils::userFromUrl)

		override val date: String
			get() = body.child(0).textNodes()[1].text().trim().substring(6)

		override val isPublic: Boolean get() = body.child(1).text() == "公开"

		override val source: String get() = body.children().last().text()

		fun newInstance(): Paste {
			return PasteData(id, user, url, date, source, isPublic)
		}
	}

	val id: String
	val user: User
	val url: String
	val date: String
	val source: String
	val isPublic: Boolean
}

data class PasteData(
		override val id: String,
		override val user: User,
		override val url: String,
		override val date: String,
		override val source: String,
		override val isPublic: Boolean
) : Paste {
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