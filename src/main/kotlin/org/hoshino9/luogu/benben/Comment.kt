package org.hoshino9.luogu.benben

import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.interfaces.HasElement
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

abstract class AbstractComment : Comment

/**
 * # **你谷** 评论类
 * 许多地方都可以用到
 *
 * @param elem 评论的HTML代码
 */
data class DefaultComment(override val elem : Element) : AbstractComment(), HasElement {
	private val children by lazy { elem.children() }
	private val userBlock by lazy { children[0] }
	private val mainBlock by lazy { children[1] }

	override val user : User by lazy {
		LuoGuUtils.getUserFromUrl(userBlock.child(0).attr("href"))
	}

	override val date : String by lazy {
		mainBlock.child(0)
				.child(0)
				.textNodes()
				.firstOrNull { it.text().isNotBlank() }
				?.text()?.trim() ?: ""
	}

	override val content : List<Node> by lazy {
		mainBlock.child(1)
				.child(0)
				.childNodes()
	}
}