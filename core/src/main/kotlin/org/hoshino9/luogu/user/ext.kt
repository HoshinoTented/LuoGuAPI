package org.hoshino9.luogu.user

import org.hoshino9.luogu.LuoGu

val LuoGu.currentUser: LoggedUserPage
	get() {
		return LoggedUserPage(uid.toInt(), this)
	}