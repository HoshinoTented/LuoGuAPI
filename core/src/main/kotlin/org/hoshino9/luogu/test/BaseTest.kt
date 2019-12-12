package org.hoshino9.luogu.test

import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.user.LoggedUser
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.user.currentUser
import java.util.Properties

abstract class BaseTest {
	companion object {
		internal val config by lazy {
			Properties().apply {
				load(BaseTest::class.java.classLoader.getResourceAsStream("user.properties"))
			}
		}
	}

	val luogu: LuoGu
	val user: LoggedUser

	init {
		val id: String? = config.getProperty("__client_id")
		val uid: String? = config.getProperty("_uid")

		if (id != null && uid != null) {
			luogu = LuoGu(id, uid)
			user = luogu.currentUser.user
		} else throw IllegalStateException("No logged in")
	}
}