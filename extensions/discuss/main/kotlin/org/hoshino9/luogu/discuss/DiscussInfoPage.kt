package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.comment.Comment
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

interface DiscussInfoPage {
	open class Factory(override val id: String, override val page: Int, val client: HttpClient = defaultClient) : DiscussInfoPage {
		val elem: Element by lazy {
			client.executeGet(url) {
				it.assert()
				Jsoup.parse(it.strData).body()
			}
		}

		override val url: String get() = "${LuoGuUtils.baseUrl}/discuss/show?postid=$id&page=$page"

		override val mainComment: Comment by lazy {
			val className = "am-comment am-comment-danger"
			DiscussComment.Factory(elem.getElementsByClass(className).first())
		}

		override val comments: List<Comment> by lazy {
			val className = "am-comment am-comment-primary"
			elem.getElementsByClass(className).map {
				DiscussComment.Factory(it)
			}
		}

		override val forum: String by lazy {
			val className = "colored"
			elem.getElementsByClass(className).first().attr("href").run(LuoGuUtils::lastValueFromUrl)
		}

		fun newInstance(): DiscussInfoPage {
			return DiscussInfoPageData(url, page, mainComment, comments, forum, id)
		}
	}

	val url: String
	val page: Int
	val mainComment: Comment
	val comments: List<Comment>
	val forum: String
	val id: String
}

data class DiscussInfoPageData(
		override val url: String,
		override val page: Int,
		override val mainComment: Comment,
		override val comments: List<Comment>,
		override val forum: String,
		override val id: String
) : DiscussInfoPage