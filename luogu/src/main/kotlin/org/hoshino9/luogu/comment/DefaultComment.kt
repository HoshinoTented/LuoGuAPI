@file:Suppress("MemberVisibilityCanBePrivate")

package org.hoshino9.luogu.comment

import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.utils.HasElement
import org.jsoup.nodes.Element
import org.jsoup.nodes.Node

/**
 * # **你谷** 评论类
 * 许多地方都可以用到
 *
 * @param elem 评论的HTML代码
 */
open class DefaultComment(override val elem : Element) : AbstractComment(), HasElement {
	protected val mainBlock : Element = run { elem.child(1) }
	protected val header : Element = run { mainBlock.child(0).child(0) }
	protected val commentMain : Element = run { mainBlock.child(1) }

	override val user : User = run {
		User(header.child(0))
	}

	override val date : String = run {
		header.textNodes().run {
			get(lastIndex - 1).toString().trim()
		}
	}

	override val content : List<Node> = run {
		commentMain
				.child(0)
				.childNodes()
	}
}