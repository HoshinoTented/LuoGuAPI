package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.comment.Comment
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

data class DiscussInfoPage(
		val url: String,
		val page: Int,
		val mainComment: Comment,
		val comments: List<Comment>,
		val forum: String,
	val id : String
) {
	open class Factory(open val id: String, open val page: Int, val client: HttpClient = defaultClient) {
		val elem: Element by lazy {
			client.executeGet(url) {
				it.assert()
				Jsoup.parse(it.strData).body()
			}
		}

		open val url: String get() = "${LuoGuUtils.baseUrl}/discuss/show?postid=$id&page=$page"

		open val mainComment: Comment by lazy {
			val className = "am-comment am-comment-danger"
			Comment.Factory(elem.getElementsByClass(className).first()).newInstance()
		}

		open val comments: List<Comment> by lazy {
			val className = "am-comment am-comment-primary"
			elem.getElementsByClass(className).map {
				DiscussComment.Factory(it).newInstance()
			}
		}

		open val forum: String by lazy {
			val className = "colored"
			elem.getElementsByClass(className).first().attr("href").run(LuoGuUtils::lastValueFromUrl)
		}

		fun newInstance(): DiscussInfoPage {
			return DiscussInfoPage(url, page, mainComment, comments, forum, id)
		}
	}
}