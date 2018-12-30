package org.hoshino9.luogu.benben

import org.hoshino9.luogu.comment.Comment
import org.jsoup.nodes.Element

object BenbenUtils {
	/**
	 * 解析benben
	 * @param list
	 * @return 返回一个犇犇列表
	 *
	 * @see Comment
	 */
	fun getBenben(list : Element) : List<Comment> {
		return list.children().mapNotNull {
			if (it.tagName() == "li") Comment(it) else null
		}
	}
}