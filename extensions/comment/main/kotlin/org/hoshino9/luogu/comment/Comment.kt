package org.hoshino9.luogu.comment

import org.hoshino9.luogu.user.User
import org.jsoup.nodes.Element
import org.jsoup.nodes.Node

data class Comment(
		val user: User,
		val date: String,
		val content: List<Node>
) {
	open class Factory(val elem: Element) {
		protected val mainBlock get() = elem.child(1)
		protected val header get() = mainBlock.child(0).child(0)
		protected val commentMain get() = mainBlock.child(1)

		open val user get() = User(header.child(0))
		open val date
			get() = header.textNodes().run {
				get(lastIndex - 1).toString().trim()
			}

		protected open val content
			get() = commentMain
					.child(0)
					.childNodes()

		fun newInstance(): Comment {
			return Comment(user, date, content)
		}
	}
}