package org.hoshino9.luogu.benben

import org.jsoup.nodes.Element

object BenbenUtils {
	/**
	 * 解析benben
	 * @param list
	 * @return 返回一个犇犇列表
	 *
	 * @see LuoGuComment
	 */
	fun getBenben(list : Element) : List<LuoGuComment> {
		return list.children().mapNotNull {
			if (it.tagName() == "li") LuoGuComment(it) else null
		}
	}
}