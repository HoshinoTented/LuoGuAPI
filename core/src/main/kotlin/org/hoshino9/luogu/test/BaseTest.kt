package org.hoshino9.luogu.test

import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.user.ILoggedUser
import org.hoshino9.luogu.user.currentUser
import rootPath
import java.nio.file.Files
import java.nio.file.Paths
import java.util.Properties

abstract class BaseTest {
	companion object {
		val resRoot = Paths.get(rootPath, "core", "src", "main", "resources")
		val verifyPath by lazy { resRoot.resolve("verify.png") }
		val configPath by lazy { resRoot.resolve("user.properties") }

		/**
		 * 请将测试的工作目录设置为项目的根目录
		 */
		val config by lazy {
			Properties().apply {
				load(configPath.toFile().inputStream())
			}
		}
	}

	lateinit var luogu: LuoGu
	lateinit var user: ILoggedUser

	init {
		loadCookie()
	}

	fun loadCookie() {
		val id: String? = config.getProperty("__client_id")
		val uid: String? = config.getProperty("_uid")

		if (id != null && uid != null) {
			luogu = LuoGu(id, uid.toInt())
			user = luogu.currentUser.user
		} else throw IllegalStateException("No logged in")
	}

	fun saveCookie() {
		config.setProperty("__client_id", luogu.clientId.value)
		config.setProperty("_uid", luogu.uid.value)
		config.store(Files.newOutputStream(configPath), null)
	}
}