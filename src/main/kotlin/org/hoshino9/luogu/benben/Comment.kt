package org.hoshino9.luogu.benben

import org.hoshino9.luogu.user.User
import org.jsoup.nodes.Element
import org.jsoup.nodes.Node

interface Comment {
	companion object {
		operator fun invoke(elem : Element) : Comment {
			return DefaultComment(elem)
		}
	}

	val user : User
	val date : String
	val content : List<Node>
}