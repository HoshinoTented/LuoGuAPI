package org.hoshino9.luogu.photo

import org.hoshino9.luogu.user.User
import org.jsoup.nodes.Element

interface Photo {
	companion object {
		@JvmName("newInstance")
		operator fun invoke(elem : Element) : Photo {
			return DefaultPhoto(elem)
		}
	}

	val id : String
	val date : String
	val user : User
	val url : String
}

