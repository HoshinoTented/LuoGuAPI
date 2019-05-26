package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.defaultClient
import org.jsoup.nodes.Element

open class DefaultDiscussNode(val elem: Element, val client: HttpClient = defaultClient) : AbstractDiscussNode() {
	private val left by lazy { elem.child(0).child(1) }
	private val mid by lazy { elem.child(1) }
	private val right by lazy { elem.child(2) }
	private val midBottom by lazy { mid.child(2) }
	private val midTop by lazy { mid.child(0) }

	override val url : String by lazy {
		baseUrl + midTop.attr("href")
	}

	override val forum : DiscussListPage by lazy {
		DiscussListPage(midBottom.child(0).attr("href").run(LuoGuUtils::lastValueFromUrl), client = client)
	}

	override val infoPage : DiscussInfoPage by lazy {
		DiscussInfoPage(id, client = client)
	}

	override val postDate : String by lazy {
		midBottom.childNode(2).toString().substring(2)
	}

	override val replyCount : Int by lazy {
		left.children().last().text().substringBefore('个').toInt()
	}

	override val user : User by lazy {
		left.child(0).run(User.Companion::invoke)
	}

	override val title : String by lazy {
		midTop.text()
	}

	override val lastCommentUser : User? by lazy {
		if (right.child(0).text() == "暂无回复") null else {
			right.child(0).run(User.Companion::invoke)
		}
	}

	override val lastCommentDate : String by lazy {
		right.child(1).text().substring(1)
	}

	override val isTopping : Boolean by lazy {
		left.child(2).text() == "置顶"
	}
}
