package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.LuoGuUtils.baseUrl

abstract class AbstractDiscussInfoPage : DiscussInfoPage {
	override val url : String get() = "$baseUrl/discuss/show?postid=$id&page=$page"

	override fun toString() : String {
		return id
	}
}