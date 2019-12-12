@file:Suppress("MemberVisibilityCanBePrivate")

import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.paste.*
import org.hoshino9.luogu.photo.photoList
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.user.currentUser
import org.junit.Before
import org.junit.Test
import java.nio.file.Files
import java.nio.file.Paths
import java.util.Properties

open class LuoGuTest {
	companion object {
		internal val testRoot = Paths.get("core/src/main/resources")
		internal val verifyPath by lazy { testRoot.resolve("verify.png") }
		internal val configPath by lazy { testRoot.resolve("user.properties") }
		internal val config by lazy {
			Properties().apply {
				load(Files.newInputStream(configPath))
			}
		}
	}

	lateinit var luogu: LuoGu

	val user by lazy { this.luogu.currentUser.user }

	val separator = "${"=".repeat(100)}\n"

	@Before
	fun loadCookie() {
		val id: String? = config.getProperty("__client_id")
		val uid: String? = config.getProperty("_uid")

		if (id != null && uid != null) {
			luogu = LuoGu(id, uid)
		} else throw IllegalStateException("No logged in")

	}

	fun saveCookie() {
		config.setProperty("__client_id", luogu.clientId)
		config.setProperty("_uid", luogu.uid)
		config.store(Files.newOutputStream(configPath), null)
	}

	//	@Test
//	@Suppress("DEPRECATION")
//	fun benbenTest() {
//		val toString : (Comment) -> String = {
//			val user = it.user
//
//			//language=TEXT
//			"""user: ${if (user is HasBadgeUser) "${user.uid}[${user.badge.text}]" else user.uid}
//date: ${it.date}
//content:
//${it.content}
//"""
//		}
//
//		user.getBenben(BenBenType.ALL).joinToString(separator = separator, transform = toString).run(::println)
//		user.getBenben(BenBenType.WATCHING).joinToString(separator = separator, transform = toString).run(::println)
//	}

	@Test
	fun userTest() {
		println("$user: ${user.name}")
	}
}