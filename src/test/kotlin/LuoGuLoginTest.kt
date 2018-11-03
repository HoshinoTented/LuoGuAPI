import org.apache.http.cookie.Cookie
import org.apache.http.impl.client.BasicCookieStore
import org.apache.http.impl.client.HttpClients
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.StatusException
import org.hoshino9.luogu.benben.BenBenType
import org.junit.Test
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.nio.file.Paths
import java.util.Properties
import java.util.Scanner

class LuoGuLoginTest {
	companion object {
		@JvmStatic
		fun main(args : Array<String>) {
			LuoGuLoginTest().run {
				login()
				saveCookie()
			}
		}
	}

	private val testRoot = "src/test/resources"
	private val verifyPath by lazy { Paths.get(testRoot, "verify.png") }
	private val cookiePath by lazy { Paths.get(testRoot, "cookie.obj") }
	private val configPath by lazy { Paths.get(testRoot, "user.properties") }
	private val config by lazy {
		Properties().apply {
			load(configPath.toFile().inputStream())
		}
	}

	private val cookieStore by lazy { BasicCookieStore() }

	private val luogu by lazy {
		val builder = HttpClients.custom()
		builder.setDefaultCookieStore(cookieStore)

		LuoGu(builder.build())
	}

	private val user by lazy { luogu.loggedUser }

	init {
		loadCookie()
	}

	private fun loadCookie() {
		if (cookiePath.toFile().exists()) {
			ObjectInputStream(cookiePath.toFile().inputStream()).use {
				it.readObject() as Cookie
			}.run(cookieStore::addCookie)
		}
	}

	private fun saveCookie() {
		ObjectOutputStream(cookiePath.toFile().outputStream()).use { out ->
			out.writeObject(cookieStore.cookies.first { it.name == "__client_id" })
		}
	}

	private fun login() {
		luogu.verifyCode(verifyPath.toFile().run(::FileOutputStream))
		println("Please input verify code")
		val verifyCode : String = Scanner(System.`in`).next()
		luogu.login(config.getProperty("account"), config.getProperty("password"), verifyCode)
	}

	@Test
	fun userTest() {
		println(user)
	}

	@Test
	fun signInTest() {
		try {
			println(user.signInStatus)
		} catch (e : StatusException) {
			println("failed, trying signing...")
			user.signIn()
			println(user.signInStatus)
		}
	}

	@Test
	fun photoListTest() {
		user.photoList().forEach {
			"url=${it.url}, date=${it.date}, uploader=${it.user}".run(::println)
		}
	}

	@Test
	fun benbenTest() {
		user.benben(BenBenType.ALL).run(::println)
		user.benben(BenBenType.WATCHING).run(::println)
	}

	@Test
	fun pasteListTest() {
		user.pasteList().run(::println)
	}

	@Test
	fun sliderPhotoTest() {
		luogu.sliderPhotos.run(::println)
	}

	@Test
	fun problemListTest() {
		luogu.problemList().forEach {
			println("${it.pid} (${it.passPercent.first} / ${it.passPercent.second})")
		}
	}
}