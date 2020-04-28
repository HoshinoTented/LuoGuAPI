@file:JvmName("UserUtils")

package org.hoshino9.luogu.user

import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.LuoGuClient

val LuoGuClient.currentUser: LoggedUserPage?
	get() {
		return LoggedUserPage(cookieUid?.toInt() ?: return null, this)
	}