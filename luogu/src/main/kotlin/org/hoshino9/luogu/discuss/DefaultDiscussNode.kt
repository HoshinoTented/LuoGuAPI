package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.utils.HasElement
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.defaultClient
import org.jsoup.nodes.Element

open class DefaultDiscussNode(override val elem : Element, val client : HttpClient = defaultClient) : AbstractDiscussNode(), HasElement {
	private val left = run { elem.child(0).child(1) }
	private val mid = run { elem.child(1) }
	private val right = run { elem.child(2) }
	private val midBottom = run { mid.child(2) }
	private val midTop = run { mid.child(0) }

	override val url : String = run {
		baseUrl + midTop.attr("href")
	}

	override val forum : DiscussListPage = run {
		DiscussListPage(midBottom.child(0).attr("href").run(LuoGuUtils::lastValueFromUrl), client = client)
	}

	override val infoPage : DiscussInfoPage = run {
		DiscussInfoPage(id, client = client)
	}

	override val postDate : String = run {
		midBottom.childNode(2).toString().substring(2)
	}

	override val replyCount : Int = run {
		left.children().last().text().substringBefore('个').toInt()
	}

	override val user : User = run {
		left.child(0).run(User.Companion::invoke)
	}

	override val title : String = run {
		midTop.text()
	}

	override val lastCommentUser : User? = run {
		if (right.child(0).text() == "暂无回复") null else {
			right.child(0).run(User.Companion::invoke)
		}
	}

	override val lastCommentDate : String = run {
		right.child(1).text().substring(1)
	}

	override val isTopping : Boolean = run {
		left.child(2).text() == "置顶"
	}
}
