package org.hoshino9.luogu.paste

import org.hoshino9.luogu.*
import org.hoshino9.luogu.interfaces.HasElement
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

interface Paste {
	val id : String
	val user : LuoGuUser
	val url : String
	val date : String
	val source : String
	val isPublic : Boolean

	fun delete(luogu : LuoGu)
}

abstract class AbstractPaste : Paste {
	override val url : String by lazy { "$baseUrl/paste/$id" }

	override fun delete(luogu : LuoGu) {
		luogu.postExecute("paste/delete/$id") { resp ->
			resp.assert()
		}
	}

	override fun equals(other : Any?) : Boolean {
		if (this === other) return true
		if (javaClass != other?.javaClass) return false

		other as AbstractPaste

		return other.id == id
	}

	override fun hashCode() : Int {
		return id.hashCode()
	}

	override fun toString() : String {
		return id
	}
}

open class BasicPaste(override val id : String) : AbstractPaste(), HasElement {
	private val body : Element by lazy { elem.getElementsByClass("lg-article").first() ?: throw HTMLParseException(elem) }

	override val elem : Element by lazy {
		defaultClient.getExecute(url) { resp ->
			resp.assert()
			val content = resp.data !!

			Jsoup.parse(content)
		}
	}

	override val user : LuoGuUser by lazy {
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
}