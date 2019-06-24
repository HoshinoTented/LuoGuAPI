package org.hoshino9.luogu.comment

import org.hoshino9.luogu.user.User
import org.jsoup.nodes.Element
import org.jsoup.nodes.Node

interface Comment {
	open class Factory(val elem: Element) : Comment {
		protected val mainBlock get() = elem.child(1)
		protected val header get() = mainBlock.child(0).child(0)
		protected val commentMain get() = mainBlock.child(1)

		override val user get() = User(header.child(0))
		override val date
			get() = header.textNodes().run {
				get(lastIndex - 1).toString().trim()
			}

		override val content
			get() = commentMain
					.child(0)
					.childNodes()

		fun newInstance(): Comment {
			return CommentData(user, date, content)
		}
	}

	val user: User
	val date: String
	val content: List<Node>
}

data class CommentData(override val user: User, override val date: String, override val content: List<Node>) : Comment