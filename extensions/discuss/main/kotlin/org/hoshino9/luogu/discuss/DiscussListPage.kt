package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

/**
 * # 讨论页面(DiscussListPage)
 * **你谷** 的讨论页面
 */
data class DiscussListPage(
		val url: String,

		/**
	 * 当前页面的页数
	 */
		val page: Int,

		/**
	 * 当前页面的讨论列表
	 */
		val discusses: List<DiscussNode>,

		/**
	 * 当前页面的标识符
	 */
		val forumName: String,

		/**
	 * 当前页面的标题
	 */
	val title : String
) {
	open class Factory(open val forumName: String, open val page: Int, val client: HttpClient = defaultClient) {
		val elem: Element by lazy {
			client.executeGet(url) { resp ->
				resp.assert()

				Jsoup.parse(resp.strData).body()
			}
		}

		open val url: String
			get() = "${LuoGuUtils.baseUrl}/discuss/lists?forumname=$forumName&page=$page"


		open val title: String
			get() {
				return "lg-toolbar".let { className ->
					elem.getElementsByClass(className).first().text()
				}
			}

		open val discusses: List<DiscussNode>
			get() {
				return "am-g lg-table-bg0 lg-table-row".let { className ->
					elem.getElementsByClass(className).map {
						DiscussNode.Factory(it, client).newInstance()
					}
				}
			}

		fun newInstance(): DiscussListPage {
			return DiscussListPage(url, page, discusses, forumName, title)
		}
	}
}