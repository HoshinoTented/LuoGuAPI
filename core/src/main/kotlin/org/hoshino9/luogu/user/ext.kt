@file:JvmName("UserUtils")

package org.hoshino9.luogu.user

import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.LuoGuClient

val LuoGu.currentUser: LoggedUserPage
	get() {
		return LoggedUserPage(uid.value.toInt(), client)
	}

val LuoGuClient.currentUser: LoggedUserPage?
	get() {
		return LoggedUserPage(cookieUid?.toInt() ?: return null, (this as LuoGuClient.Companion.Impl).client)       // TODO Rubbish Code
	}