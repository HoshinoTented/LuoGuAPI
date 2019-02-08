package org.hoshino9.luogu.problem.tags

import org.hoshino9.luogu.color.luoguColor
import org.hoshino9.luogu.problem.Problem
import org.hoshino9.luogu.tag.LuoGuTag
import org.jsoup.nodes.Element

fun parseTags(element : Element) : List<LuoGuTag> {
	return element.getElementsByClass("am-badge").map { elem ->
		if ("lg-tag" in elem.classNames()) {
			LuoGuTag(elem.text(), elem.attr("data-tagid").toInt(), elem.luoguColor !!.toColor())
		} else Problem.Difficulty(elem)
	}
}