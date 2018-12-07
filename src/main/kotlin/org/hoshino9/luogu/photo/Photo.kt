package org.hoshino9.luogu.photo

import org.hoshino9.luogu.*
import org.hoshino9.luogu.user.User
import org.jsoup.nodes.Element

interface Photo {
	val id : String
	val date : String
	val user : User
	val url : String

	fun delete(luogu : LuoGu)

	class Builder {
		private var elem : Element? = null

		fun element(elem : Element) : Builder = apply {
			this.elem = elem
		}

		fun build() : Photo {
			return ParsedPhoto(elem ?: throw NullPointerException("Builder::elem == null"))
		}
	}
}

