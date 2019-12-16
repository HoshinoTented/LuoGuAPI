package org.hoshino9.luogu.test

import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.user.ILoggedUser
import org.hoshino9.luogu.user.LoggedUser
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.user.currentUser
import java.io.File
import java.nio.file.Paths
import java.util.Properties

abstract class BaseTest {
	companion object {
		internal val config by lazy {
			Properties().apply {
				load(File("core/src/main/resources/user.properties").inputStream())
			}
		}
	}

	val luogu: LuoGu
	val user: ILoggedUser

	init {
		val id: String? = config.getProperty("__client_id")
		val uid: String? = config.getProperty("_uid")

		if (id != null && uid != null) {
			luogu = LuoGu(id, uid)
			user = luogu.currentUser.user
		} else throw IllegalStateException("No logged in")
	}
}