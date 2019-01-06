package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

open class DefaultDiscussListPage(override val forumName : String, override val page : Int, override val client : HttpClient = defaultClient) : AbstractDiscussListPage(), HasElement, NeedClient {
	override val elem : Element by lazy {
		client.executeGet(url) { resp ->
			resp.assert()

			Jsoup.parse(resp.strData).body()
		}
	}

	override val title : String by lazy {
		"lg-toolbar".let { className ->
			elem.getElementsByClass(className).first().text()
		}
	}

	override val discusses : List<DiscussNode> by lazy {
		"am-g lg-table-bg0 lg-table-row".let { className ->
			elem.getElementsByClass(className).map {
				DiscussNode(it, client)
			}
		}
	}
}