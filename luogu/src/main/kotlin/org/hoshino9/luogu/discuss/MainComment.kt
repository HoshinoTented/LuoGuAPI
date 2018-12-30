package org.hoshino9.luogu.discuss

import org.jsoup.nodes.Element

open class MainComment(elem : Element) : DiscussComment(elem) {
	val replyCount : Int by lazy {
		elem.attr("data-reply-count").toInt()
	}
}