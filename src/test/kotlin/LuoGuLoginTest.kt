import org.apache.http.cookie.Cookie
import org.apache.http.impl.client.BasicCookieStore
import org.apache.http.impl.client.HttpClients
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.StatusException
import org.hoshino9.luogu.benben.BenBenType
import org.hoshino9.luogu.problems.ParsedProblem
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.nio.file.Paths
import java.util.Properties
import java.util.Scanner

class LuoGuLoginTest {
	companion object {
		private const val testRoot = "src/test/resources"
		private val verifyPath by lazy { Paths.get(testRoot, "verify.png") }
		private val cookiePath by lazy { Paths.get(testRoot, "cookie.obj") }
		private val configPath by lazy { Paths.get(testRoot, "user.properties") }
		private val config by lazy {
			Properties().apply {
				load(configPath.toFile().inputStream())
			}
		}

		@JvmStatic
		fun main(args : Array<String>) {
			val builder = HttpClients.custom()
			val cookieStore = BasicCookieStore()
			builder.setDefaultCookieStore(cookieStore)

			if (cookiePath.toFile().exists()) {
				ObjectInputStream(cookiePath.toFile().inputStream()).use {
					it.readObject() as Cookie
				}.run(cookieStore::addCookie)
			}

			// main test
			LuoGu(builder.build()).apply {
				if (cookieStore.cookies.isEmpty()) {
					verifyCode(verifyPath.toFile().run(::FileOutputStream))
					println("Please input verify code")
					val verifyCode : String = Scanner(System.`in`).next()
					login(config.getProperty("account"), config.getProperty("password"), verifyCode)
				}

				loggedUser.let { user ->
					println(user)

					try {
						println(user.signInStatus)
					} catch (e : StatusException) {
						println("auto signing...")
						user.signIn()
						println(user.signInStatus)
					}

					user.photoList().forEach {
						println("url=${it.url}, date=${it.date}, uploader=${it.uid}")
					}

					println(user.benben(BenBenType.ALL))
				}

				println(this.sliderPhotos)
				this.problemList().forEach {
					it as ParsedProblem
					println("${it.pid} with ${it.passPercent.first} / ${it.passPercent.second}")
				}
			}

			//save cookie
			ObjectOutputStream(cookiePath.toFile().outputStream()).use { out ->
				out.writeObject(cookieStore.cookies.first { it.name == "__client_id" })
			}
		}
	}
}