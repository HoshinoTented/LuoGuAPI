package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.comment.DefaultComment
import org.hoshino9.luogu.user.User
import org.jsoup.nodes.Element
import org.jsoup.nodes.Node

open class DiscussComment(elem : Element) : DefaultComment(elem) {
	override val user : User = run {
		getUserFromDiscussHeader(header)
	}

	override val content : List<Node> = run {
		commentMain.children()
	}
}