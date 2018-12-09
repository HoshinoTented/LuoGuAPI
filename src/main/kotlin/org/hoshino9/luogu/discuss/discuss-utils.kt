package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.user.User
import org.jsoup.nodes.Element

fun getUserFromDiscussHeader(elem : Element) : User {
	return elem.clone().let { clonedElem ->
		Element("Internal").also { newElem ->
			clonedElem.children().apply {
				removeAt(0)
				removeAt(lastIndex)
			}.forEach {
				newElem.appendChild(it)
			}
		}
	}.run(User.Companion::invoke)
}