package org.hoshino9.luogu.paste

import org.hoshino9.luogu.*
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

open class DefaultPaste(override val id : String, val client : HttpClient = defaultClient) : AbstractPaste(), HasElement {
	private val body : Element by lazy { elem.getElementsByClass("lg-article").first() ?: throw HTMLParseException(elem) }

	override val elem : Element by lazy {
		client.getExecute(url) { resp ->
			resp.assert()
			val content = resp.data !!

			Jsoup.parse(content)
		}
	}

	override val user : User by lazy {
		body.child(0).child(0).attr("href").run(LuoGuUtils::getUserFromUrl)
	}

	override val date : String by lazy {
		body.child(0).textNodes()[1].text().trim().substring(6)
	}

	override val isPublic : Boolean by lazy {
		body.child(1).text() == "公开"
	}

	override val source : String by lazy {
		body.children().last().text()
	}

	override fun toString() : String {
		return id
	}

	override fun equals(other : Any?) : Boolean {
		if (this === other) return true
		if (other !is DefaultPaste) return false

		if (id != other.id) return false

		return true
	}

	override fun hashCode() : Int {
		return id.hashCode()
	}
}