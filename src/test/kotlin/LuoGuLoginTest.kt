import okhttp3.Cookie
import okhttp3.HttpUrl
import org.hoshino9.luogu.*
import org.hoshino9.luogu.benben.BenBenType
import org.hoshino9.luogu.benben.LuoGuComment
import org.hoshino9.luogu.results.LuoGuSignedInStatus
import org.junit.Test
import java.io.FileOutputStream
import java.nio.file.Paths
import java.util.Properties
import java.util.Scanner
import kotlin.system.measureTimeMillis

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

	private val testRoot = Paths.get("src/test/resources")
	private val verifyPath by lazy { testRoot.resolve("verify.png") }
	private val cookiePath by lazy { testRoot.resolve("cookie.obj") }
	private val configPath by lazy { testRoot.resolve("user.properties") }
	private val config by lazy {
		Properties().apply {
			load(configPath.toFile().inputStream())
		}
	}

	private val luogu by lazy {
		LuoGu()
	}

	private val user by lazy { luogu.loggedUser }

	private val separator = "${"=".repeat(100)}\n"

	init {
		loadCookie()
	}

	private fun loadCookie() {
		val id : String? = config.getProperty("__client_id")

		if (id != null)
			luogu.client.cookieJar()
					.saveFromResponse(HttpUrl.get("https://www.luogu.org"), listOf(Cookie.Builder()
							.domain("www.luogu.org")
							.name("__client_id")
							.value(id)
							.build()
					))
	}

	private fun saveCookie() {
		val id = luogu.client.cookieJar().loadForRequest(HttpUrl.get("https://www.luogu.org"))
				.first { it.name() == "__client_id" }.value()

		config.setProperty("__client_id", id)
		config.store(configPath.toFile().outputStream(), null)
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
		val toString : (LuoGuSignedInStatus.Thing) -> String = {
			"${it.name}: ${it.description}"
		}

		val status = try {
			user.signInStatus
		} catch (e : StatusException) {
			println("failed, trying signing...")
			user.signIn()
			user.signInStatus
		}

		//language=TEXT
		"""${status.qian.show}
宜:
${status.goods.joinToString(separator = "\n", transform = toString)}

忌:
${status.bads.joinToString(separator = "\n", transform = toString)}
""".run(::println)

	}

	@Test
	fun photoListTest() {
		user.photoList().joinToString {
			//language=TEXT
			"""user: ${it.user}
url: ${it.url}
date: ${it.date}
${it.user}
"""
		}.run(::println)
	}

	@Test
	fun benbenTest() {
		val toString : (LuoGuComment) -> String = {
			//language=TEXT
			"""user: ${it.user}
date: ${it.date}
content:
${it.content}
"""
		}

		user.benben(BenBenType.ALL).joinToString(separator = separator, transform = toString).run(::println)
		user.benben(BenBenType.WATCHING).joinToString(separator = separator, transform = toString).run(::println)
	}

	@Test
	fun pasteListTest() {
		user.pasteList().joinToString(separator = separator) {
			//language=TEXT
			"""user: ${it.user}
date: ${it.date}
is public: ${it.isPublic}
source:
${it.source}
"""
		}.run(::println)
	}

	@Test
	fun sliderPhotoTest() {
		luogu.sliderPhotos.forEach {
			val time = measureTimeMillis {
				defaultClient.getExecute(it.second) { resp ->
					resp.assert()
					resp.body() !!.byteStream().copyTo(testRoot.resolve(it.second.hashCode().toString() + ".png").toFile().outputStream())
				}
			}

			println("used $time ms")
		}
	}

	@Test
	fun problemListTest() {
		luogu.problemList().forEach {
			println("${it.id} (${it.passPercent.first} / ${it.passPercent.second})")
		}
	}
}