package org.hoshino9.luogu.user

import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.color.luoguFrontColor
import org.hoshino9.luogu.tag.ColoredLuoGuTag
import org.jsoup.nodes.Element
import java.awt.Color

// FIXME: 这个也得删，头衔信息应该从 User 里拿，但你谷好像没有提供 API

open class HasBadgeUser(uid: String, val badge: ColoredLuoGuTag) : User(uid) {
	companion object {
		class UserBadge(name: String, color: Color) : ColoredLuoGuTag(name, - 1, color)

		@JvmName("newInstance")
		operator fun invoke(element : Element) : HasBadgeUser {
			val name = element.child(0)
			val badge = element.child(1)

			return HasBadgeUser(LuoGuUtils.userFromUrl(name.attr("href")).uid, UserBadge(badge.text(), name.luoguFrontColor !!.toColor()))
		}
	}
}