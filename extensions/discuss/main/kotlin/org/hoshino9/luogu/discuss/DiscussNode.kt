package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.defaultClient
import org.jsoup.nodes.Element

interface DiscussNode {
	open class Factory(val elem: Element, val client: HttpClient = defaultClient) : DiscussNode {
		protected val left get() = elem.child(0).child(1)
		protected val mid get() = elem.child(1)
		protected val right get() = elem.child(2)
		protected val midBottom get() = mid.child(2)
		protected val midTop get() = mid.child(0)

		override val url: String get() = LuoGuUtils.baseUrl + midTop.attr("href")
		override val id: String get() = LuoGuUtils.lastValueFromUrl(url)
		override val forum: DiscussListPage get() = DiscussListPage.Factory(midBottom.child(0).attr("href").run(LuoGuUtils::lastValueFromUrl), 1, client)
		override val infoPage: DiscussInfoPage get() = DiscussInfoPage.Factory(id, 1, client)
		override val postDate: String get() = midBottom.childNode(2).toString().substring(2)
		override val replyCount: Int get() = left.children().last().text().substringBefore('个').toInt()
		override val user: User get() = left.child(0).run(User.Companion::invoke)
		override val title: String get() = midTop.text()
		override val lastCommentUser: User? by lazy {
			if (right.child(0).text() == "暂无回复") null else {
				right.child(0).run(User.Companion::invoke)
			}
		}

		override val lastCommentDate: String get() = right.child(1).text().substring(1)
		override val isTopping: Boolean get() = left.child(2).text() == "置顶"

		fun newInstance(): DiscussNode {
			return DiscussNodeData(url, isTopping, id, replyCount, user, forum, title, postDate, lastCommentUser, lastCommentDate, infoPage)
		}
	}

	val url: String
	val isTopping: Boolean
	val id: String
	val replyCount: Int
	val user: User
	val forum: DiscussListPage
	val title: String
	val postDate: String
	val lastCommentUser: User?
	val lastCommentDate: String
	val infoPage: DiscussInfoPage
}

data class DiscussNodeData(override val url: String, override val isTopping: Boolean, override val id: String, override val replyCount: Int, override val user: User, override val forum: DiscussListPage, override val title: String, override val postDate: String, override val lastCommentUser: User?, override val lastCommentDate: String, override val infoPage: DiscussInfoPage) : DiscussNode