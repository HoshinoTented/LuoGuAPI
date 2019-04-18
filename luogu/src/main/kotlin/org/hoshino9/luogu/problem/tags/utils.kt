package org.hoshino9.luogu.problem.tags

import org.hoshino9.luogu.color.luoguBadgeColor
import org.hoshino9.luogu.problem.Problem
import org.hoshino9.luogu.tag.ColoredLuoGuTag
import org.jsoup.nodes.Element

fun parseTags(element: Element): List<ColoredLuoGuTag> {
	return element.getElementsByClass("am-badge").map { elem ->
		if ("lg-tag" in elem.classNames()) {
			ColoredLuoGuTag(elem.text(), elem.attr("data-tagid").toInt(), elem.luoguBadgeColor !!.toColor())
		} else Problem.Difficulty(elem)
	}
}