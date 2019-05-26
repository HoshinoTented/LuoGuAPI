package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.defaultClient

/**
 * # 讨论页面(DiscussListPage)
 * **你谷** 的讨论页面
 */
interface DiscussListPage {
	companion object {
		@JvmName("newInstance")
		operator fun invoke(forumName : String, page : Int = 1, client : HttpClient = defaultClient) : DiscussListPage {
			return DefaultDiscussListPage(forumName, page, client)
		}
	}

	val url: String

	/**
	 * 当前页面的页数
	 */
	val page : Int

	/**
	 * 当前页面的讨论列表
	 */
	val discusses : List<DiscussNode>

	/**
	 * 当前页面的标识符
	 */
	val forumName : String

	/**
	 * 当前页面的标题
	 */
	val title : String
}