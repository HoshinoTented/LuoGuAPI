package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.defaultClient
import org.jsoup.nodes.Element

data class DiscussNode(
		val url: String,
		val isTopping: Boolean,
		val id: String,
		val replyCount: Int,
		val user: User,
		val forum: DiscussListPage,
		val title: String,
		val postDate: String,
		val lastCommentUser: User?,
		val lastCommentDate: String,
		val infoPage: DiscussInfoPage
) {
	open class Factory(val elem: Element, val client: HttpClient = defaultClient) {
		protected open val left get() = elem.child(0).child(1)
		protected open val mid get() = elem.child(1)
		protected open val right get() = elem.child(2)
		protected open val midBottom get() = mid.child(2)
		protected open val midTop get() = mid.child(0)

		open val url: String get() = LuoGuUtils.baseUrl + midTop.attr("href")
		open val id: String get() = LuoGuUtils.lastValueFromUrl(url)
		open val forum: DiscussListPage get() = DiscussListPage.Factory(midBottom.child(0).attr("href").run(LuoGuUtils::lastValueFromUrl), 1, client).newInstance()
		open val infoPage: DiscussInfoPage get() = DiscussInfoPage.Factory(id, 1, client).newInstance()
		open val postDate: String get() = midBottom.childNode(2).toString().substring(2)
		open val replyCount: Int get() = left.children().last().text().substringBefore('个').toInt()
		open val user: User get() = left.child(0).run(User.Companion::invoke)
		open val title: String get() = midTop.text()
		open val lastCommentUser: User? by lazy {
			if (right.child(0).text() == "暂无回复") null else {
				right.child(0).run(User.Companion::invoke)
			}
		}

		protected open val lastCommentDate: String get() = right.child(1).text().substring(1)
		protected open val isTopping: Boolean get() = left.child(2).text() == "置顶"

		fun newInstance(): DiscussNode {
			return DiscussNode(url, isTopping, id, replyCount, user, forum, title, postDate, lastCommentUser, lastCommentDate, infoPage)
		}
	}
}