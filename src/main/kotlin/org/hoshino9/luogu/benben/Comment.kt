package org.hoshino9.luogu.benben

import org.hoshino9.luogu.User
import org.jsoup.nodes.Element
import org.jsoup.nodes.Node

/**
 * # **你谷** 评论类
 * 许多地方都可以用到
 *
 * @param user 发送该评论的用户
 * @param date 发送的时间
 * @param content 本体, 是一个 Node 的列表
 */
data class Comment(val user : User, val date : String, val content : List<Node>) {
	companion object Utils {
		@JvmName("newInstance")
		operator fun invoke(element : Element) : Comment {
			val children = element.children()
			val userBlock = children.getOrNull(0) ?: throw NoSuchElementException()
			val mainBlock = children.getOrNull(1) ?: throw NoSuchElementException()

			//user block
			val spaceLink = userBlock.children().first()?.attr("href") ?: throw NoSuchElementException()
			val uid = spaceLink.substring(spaceLink.lastIndexOf('=') + 1)

			//main block
			val header = mainBlock.children().first() ?: throw NoSuchElementException()
			val comment = mainBlock.children().getOrNull(1) ?: throw NoSuchElementException()

			//header date
			val date = header.children().first()?.textNodes()?.firstOrNull { it.text().isNotBlank() }?.text()?.trim() ?: ""

			//comment
			val content = comment.children().first()?.childNodes() ?: throw NoSuchElementException()

			return Comment(User(uid), date, content)
		}
	}
}