@file:JvmName("DiscussUtils")

package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.LuoGuUtils

val LuoGu.posts: List<DiscussInfoPage>
	get() {
		return page.getElementsByClass("am-u-lg-3 am-u-md-4 lg-right").first().getElementsByTag("a").map {
			@Suppress("ReplaceSingleLineLet")
			it.attr("href").run(LuoGuUtils::lastValueFromUrl).let { id ->
				DiscussInfoPage.Factory(id, 1, client).newInstance()
			}
		}
	}