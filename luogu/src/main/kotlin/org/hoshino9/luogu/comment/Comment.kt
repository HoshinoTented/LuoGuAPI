package org.hoshino9.luogu.comment

import org.hoshino9.luogu.discuss.DiscussComment
import org.hoshino9.luogu.discuss.MainComment
import org.hoshino9.luogu.user.User
import org.jsoup.nodes.Element
import org.jsoup.nodes.Node

interface Comment {
	companion object {
		@JvmName("newInstance")
		operator fun invoke(elem : Element) : Comment {
			return if (elem.tagName() == "article") {
				when (val className = elem.className()) {
					"am-comment am-comment-danger" -> ::MainComment
					"am-comment am-comment-primary" -> ::DiscussComment

					else -> throw IllegalArgumentException("Unexpected className: $className")
				}.invoke(elem)
			} else DefaultComment(elem)
		}
	}

	val user : User
	val date : String
	val content : List<Node>
}