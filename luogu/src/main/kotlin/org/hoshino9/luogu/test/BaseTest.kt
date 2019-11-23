package org.hoshino9.luogu.test

import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.user.LoggedUser
import java.nio.file.Files
import java.nio.file.Paths
import java.util.Properties

abstract class BaseTest {
	companion object {
		internal val testRoot = Paths.get("testResources")
		internal val verifyPath by lazy { testRoot.resolve("verify.png") }
		internal val configPath by lazy { testRoot.resolve("user.properties") }
		internal val config by lazy {
			Properties().apply {
				load(Files.newInputStream(configPath))
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
			user = luogu.loggedUser
		} else throw IllegalStateException("No logged in")
	}
}