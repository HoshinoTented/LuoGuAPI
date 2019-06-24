package org.hoshino9.luogu.photo

import org.hoshino9.luogu.HTMLParseException
import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.MatchException
import org.hoshino9.luogu.user.User
import org.jsoup.nodes.Element

interface Photo {
	open class Factory(val elem: Element) : Photo {
		private val leftElem: Element get() = elem.child(0)
		private val rightElem: Element get() = elem.child(1)
		private val urlRegex = Regex("""https://cdn.luogu.org/upload/pic/(\d+)\.(jpg|png|gif)""")

		override val id: String by lazy {
			urlRegex.matchEntire(url)?.run {
				groupValues[1]
			} ?: throw MatchException(urlRegex, url)
		}

		override val date: String by lazy {
			leftElem.child(2).run {
				textNodes().lastOrNull { it.text().isNotBlank() }?.text()?.trim() ?: throw HTMLParseException(this)
			}
		}

		override val user: User by lazy {
			leftElem.child(2).child(0).attr("href").run(LuoGuUtils::userFromUrl)
		}

		override val url: String by lazy {
			rightElem.child(0).child(0).text()
		}

		fun newInstance(): Photo {
			return PhotoData(id, date, user, url)
		}
	}

	val id: String
	val date: String
	val user: User
	val url: String
}

data class PhotoData(override val id: String, override val date: String, override val user: User, override val url: String) : Photo {
	override fun equals(other: Any?): Boolean {
		if (this === other) return true
		if (other !is Photo) return false

		if (id != other.id) return false

		return true
	}

	override fun hashCode(): Int {
		return id.hashCode()
	}
}

