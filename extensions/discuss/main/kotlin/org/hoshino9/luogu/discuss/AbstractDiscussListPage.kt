@file:Suppress("unused")

package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.LuoGuUtils.baseUrl

abstract class AbstractDiscussListPage : DiscussListPage {
	override val url : String
		get() = "$baseUrl/discuss/lists?forumname=$forumName&page=$page"

	override fun toString() : String {
		return forumName
	}
}