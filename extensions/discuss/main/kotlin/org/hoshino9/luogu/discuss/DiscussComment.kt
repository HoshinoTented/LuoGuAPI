package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.comment.Comment
import org.hoshino9.luogu.user.User
import org.jsoup.nodes.Element
import org.jsoup.nodes.Node

interface DiscussComment {
	open class Factory(elem: Element) : Comment.Factory(elem) {
		override val user: User
			get() {
				return getUserFromDiscussHeader(header)
			}

		override val content: List<Node>
			get() {
				return commentMain.children()
			}
	}
}

