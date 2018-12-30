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
	protected val mainBlock : Element by lazy { elem.child(1) }
	protected val header : Element by lazy { mainBlock.child(0).child(0) }
	protected val commentMain : Element by lazy { mainBlock.child(1) }

	override val user : User by lazy {
		User(header.child(0))
	}

	override val date : String by lazy {
		header.textNodes().run {
			get(lastIndex - 1).toString().trim()
		}
	}

	override val content : List<Node> by lazy {
		commentMain
				.child(0)
				.childNodes()
	}
}