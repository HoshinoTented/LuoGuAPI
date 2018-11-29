import okhttp3.HttpUrl
import org.hoshino9.luogu.*
import org.hoshino9.luogu.benben.BenBenType
import org.hoshino9.luogu.benben.Comment
import org.hoshino9.luogu.results.SignedInStatus
import org.junit.Before
import org.junit.Test
import java.io.FileOutputStream
import java.nio.file.Paths
import java.util.Properties
import java.util.Scanner

fun main(args : Array<String>) {
	LuoGuTest().run {
		login()
		println("logged in")

		saveCookie()
		println("save cookie")
	}
}

class LuoGuTest {
	companion object {
		private val testRoot = Paths.get("src/test/resources")
		private val verifyPath by lazy { testRoot.resolve("verify.png") }
		private val configPath by lazy { testRoot.resolve("user.properties") }
		internal val config by lazy {
			Properties().apply {
				load(configPath.toFile().inputStream())
			}
		}
	}

	internal lateinit var luogu : LuoGu

	private val user by lazy { this.luogu.loggedUser }

	private val separator = "${"=".repeat(100)}\n"

	@Before
	fun loadCookie() {
		val id : String? = config.getProperty("__client_id")

		if (id != null) {
			luogu = LuoGu(id)
		} else throw IllegalStateException("No logged in")

	}

	internal fun login() {
		luogu = LuoGu()
		luogu.verifyCode(verifyPath.toFile().run(::FileOutputStream))
		println("Please input verify code")
		val verifyCode : String = Scanner(System.`in`).next()
		luogu.login(config.getProperty("account"), config.getProperty("password"), verifyCode)
	}

	internal fun saveCookie() {
		val id = luogu.client.cookieJar().loadForRequest(HttpUrl.get("https://www.luogu.org"))
				.first { it.name() == "__client_id" }.value()

		config.setProperty("__client_id", id)
		config.store(configPath.toFile().outputStream(), null)
	}

	@Test
	fun userTest() {
		println(user)
	}

	@Test
	fun signInTest() {
		val toString : (SignedInStatus.Thing) -> String = {
			"${it.name}: ${it.description}"
		}

		val status = try {
			user.signInStatus
		} catch (e : IllegalStateException) {
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
		val toString : (Comment) -> String = {
			//language=TEXT
			"""user: ${it.user}
date: ${it.date}
content:
${it.content}
"""
		}

		user.getBenben(BenBenType.ALL).joinToString(separator = separator, transform = toString).run(::println)
		user.getBenben(BenBenType.WATCHING).joinToString(separator = separator, transform = toString).run(::println)
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
			println(it)
//			val time = measureTimeMillis {
//				defaultClient.getExecute(it.img) { resp ->
//					resp.assert()
//					resp.body() !!.byteStream().copyTo(testRoot.resolve(it.img.hashCode().toString() + ".png").toFile().outputStream())
//				}
//			}
//
//			println("used $time ms")
		}
	}

	@Test
	fun problemListTest() {
		luogu.problemList().forEach {
			println("${it.id} (${it.passPercent.first} / ${it.passPercent.second})")
		}
	}

	@Test
	fun training() {
		luogu.trainingPage.trainingBlocks.forEach {
			println(it.name)
			it.trainings.forEach { training ->
				print("[${training.status.content}]")
				println(training.name)
				training.problems.forEach { problem ->
					println(problem.id)
				}
			}

			println(separator)
		}
	}
}