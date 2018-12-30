package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.LuoGuUtils

abstract class AbstractDiscussNode : DiscussNode {
	override val id : String
		get() = LuoGuUtils.lastValueFromUrl(url)

	override fun toString() : String {
		return id
	}
}