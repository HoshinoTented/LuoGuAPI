package org.hoshino9.luogu.benben

import org.hoshino9.luogu.LuoGuUser
import org.jsoup.nodes.Element
import org.jsoup.nodes.Node

data class LuoGuComment(val user : LuoGuUser, val date : String, val content : List<Node>) {
	companion object Utils {
		@JvmName("newInstance")
		operator fun invoke(element : Element) : LuoGuComment {
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

			return LuoGuComment(LuoGuUser(uid), date, content)
		}
	}
}