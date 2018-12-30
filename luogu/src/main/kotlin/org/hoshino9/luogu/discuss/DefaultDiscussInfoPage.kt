package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.comment.Comment
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

open class DefaultDiscussInfoPage(override val id : String, override val page : Int, val client : HttpClient = defaultClient) : AbstractDiscussInfoPage(), HasElement {
	override val elem : Element by lazy {
		client.executeGet(url) {
			it.assert()
			Jsoup.parse(it.data).body()
		}
	}

	override val mainComment : MainComment by lazy {
		val className = "am-comment am-comment-danger"
		Comment(elem.getElementsByClass(className).first()) as MainComment
	}

	override val comments : List<DiscussComment> by lazy {
		val className = "am-comment am-comment-primary"
		elem.getElementsByClass(className).map {
			Comment(it) as DiscussComment
		}
	}

	override val forum : String by lazy {
		val className = "colored"
		elem.getElementsByClass(className).first().attr("href").run(LuoGuUtils::lastValueFromUrl)
	}
}