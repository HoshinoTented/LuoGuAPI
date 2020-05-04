package org.hoshino9.luogu.test

import com.google.gson.JsonObject
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.LuoGuClient
import org.hoshino9.luogu.user.LoggedUser
import org.hoshino9.luogu.user.currentUser
import rootPath
import java.nio.file.Files
import java.nio.file.Paths
import java.util.Properties

abstract class BaseTest {
	companion object {
		val root = Paths.get(rootPath)
		val verifyPath by lazy { root.resolve("verify.png") }
		val configPath by lazy { root.resolve("user.properties") }

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
	lateinit var user: LoggedUser

	lateinit var client: LuoGuClient

	init {
		runBlocking {
			loadCookie()
		}
	}

	suspend fun loadCookie() {
		val id: String? = config.getProperty("__client_id")
		val uid: String? = config.getProperty("_uid")

		if (id != null && uid != null) {
			luogu = LuoGu(id, uid.toInt())
			client = LuoGuClient(id, uid.toInt())
			user = client.currentUser !!.user
		} else {
			System.err.println("No logged in.")

			luogu = LuoGu()
			client = LuoGuClient()
		}
	}

	fun saveCookie() {
		config.setProperty("__client_id", client.cookieClientId)
		config.setProperty("_uid", client.cookieUid)
		config.store(Files.newOutputStream(configPath), null)
	}
}