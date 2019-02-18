package org.hoshino9.luogu.user

import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.color.luoguBadgeColor
import org.hoshino9.luogu.color.luoguFrontColor
import org.hoshino9.luogu.tag.LuoGuTag
import org.jsoup.nodes.Element
import java.awt.Color

open class HasBadgeUser(uid : String, val badge : LuoGuTag) : User(uid) {
	companion object {
		class UserBadge(name : String, color : Color) : LuoGuTag(name, - 1, color)

		@JvmName("newInstance")
		operator fun invoke(element : Element) : HasBadgeUser {
			val name = element.child(0)
			val badge = element.child(1)

			return HasBadgeUser(LuoGuUtils.getUserFromUrl(name.attr("href")).uid, UserBadge(badge.text(), name.luoguFrontColor !!.toColor()))
		}
	}
}