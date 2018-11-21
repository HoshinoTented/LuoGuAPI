package org.hoshino9.luogu.photo

import org.hoshino9.luogu.*
import org.hoshino9.luogu.interfaces.HasElement
import org.jsoup.nodes.Element

interface LuoGuPhoto {
	val id : String
	val date : String
	val user : LuoGuUser
	val url : String

	fun delete(luogu : LuoGu)

	class Builder {
		private var elem : Element? = null

		fun element(elem : Element) : Builder = apply {
			this.elem = elem
		}

		fun build() : LuoGuPhoto {
			return ParsedLuoGuPhoto(elem ?: throw NullPointerException("Builder::elem == null"))
		}
	}
}

abstract class AbstractLuoGuPhoto : LuoGuPhoto {
	private val urlRegex = Regex("""https://cdn.luogu.org/upload/pic/(\d+)\.(jpg|png|gif)""")

	override val id : String by lazy {
		urlRegex.matchEntire(url)?.run {
			groupValues[1]
		} ?: throw MatchException(urlRegex, url)
	}

	override fun delete(luogu : LuoGu) {
		luogu.postExecute("app/upload?delete=1&uploadid=$id") {
			it.assert()
		}
	}

	override fun equals(other : Any?) : Boolean {
		if (this === other) return true
		if (javaClass != other?.javaClass) return false

		other as AbstractLuoGuPhoto

		return other.id == id
	}

	override fun hashCode() : Int {
		return id.hashCode()
	}

	override fun toString() : String {
		return id
	}
}

open class ParsedLuoGuPhoto(override val elem : Element) : AbstractLuoGuPhoto(), HasElement {
	private val leftElem : Element by lazy { elem.child(0) }
	private val rightElem : Element by lazy { elem.child(1) }

	override val date : String by lazy {
		leftElem.child(2).run {
			textNodes().lastOrNull { it.text().isNotBlank() }?.text()?.trim() ?: throw HTMLParseException(this)
		}
	}

	override val user : LuoGuUser by lazy {
		leftElem.child(2).child(0).attr("href").run(LuoGuUtils::getUserFromUrl)
	}

	override val url : String by lazy {
		rightElem.child(0).child(0).text()
	}
}