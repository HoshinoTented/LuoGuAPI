import org.apache.http.cookie.Cookie
import org.apache.http.impl.client.BasicCookieStore
import org.apache.http.impl.client.HttpClients
import org.apache.http.impl.cookie.BasicClientCookie
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.LuoGuException
import org.hoshino9.luogu.LuoGuUser
import org.hoshino9.luogu.LuoGuUserException
import org.hoshino9.luogu.problems.Solution
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.OutputStream
import java.nio.file.Paths
import java.util.Calendar
import java.util.Date
import java.util.Properties
import java.util.Scanner

class LuoGuLoginTest {
	companion object {
		private const val testRoot = "src/test/resources"

		@JvmStatic
		fun main(args : Array<String>) {
			val cookieObj = Paths.get(testRoot, "cookie.obj")

			val builder = HttpClients.custom()
			val cookieStore = BasicCookieStore()
			builder.setDefaultCookieStore(cookieStore)

			val configPath = Paths.get("$testRoot/user.properties")
			val config = Properties().apply {
				load(configPath.toFile().inputStream())
			}

			if (cookieObj.toFile().exists()) {
				ObjectInputStream(cookieObj.toFile().inputStream()).use {
					it.readObject() as Cookie
				}.run(cookieStore::addCookie)
			}

			LuoGu(builder.build()).apply {
				if (cookieStore.cookies.isEmpty()) {
					verifyCode(Paths.get("$testRoot/verify.png").toFile().run(::FileOutputStream))
					println("Please input verify code")
					val verifyCode : String = Scanner(System.`in`).next()
					login(config.getProperty("account"), config.getProperty("password"), verifyCode).run {
						if (code != 200) throw LuoGuException(this@apply, message)
					}
				}

				LuoGuUser(this).let { user ->
					val id = user.paste("中文, Special character <>&^%")
					println("https://www.luogu.org/paste/$id")
					user.deletePaste(id)
				}
			}

			//save
			ObjectOutputStream(cookieObj.toFile().outputStream()).use { out ->
				out.writeObject(cookieStore.cookies.first { it.name == "__client_id" })
			}
		}
	}
}