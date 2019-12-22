import io.ktor.client.features.cookies.cookies
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.test.BaseTest
import org.hoshino9.luogu.test.BaseTest.Companion.config
import org.hoshino9.luogu.test.BaseTest.Companion.verifyPath
import java.nio.file.Files
import java.util.Scanner

object LuoGuTest : BaseTest()

suspend fun main() {
	LuoGuTest.run {
		login()
		println("logged in: ${user.uid}")

		val unlockType = luogu.needUnlock

		if (unlockType != null) {
			println("need unlock")
			val code = Scanner(System.`in`).nextLine()

			when (unlockType) {
				"2fa" -> {
					luogu.unlock(code)
				}
			}

			println("unlocked")
		}

		saveCookie()
		println("save cookie")
	}
}

suspend fun LuoGuTest.login() {
	luogu = LuoGu()

	Files.newOutputStream(verifyPath.also { path ->
		path.toFile().let {
			if (it.exists().not()) {
				it.parentFile.mkdirs()
				it.createNewFile()
			}
		}
	}).write(luogu.verifyCode())

	println("Please input verify code")
	val verifyCode: String = Scanner(System.`in`).next()
	luogu.login(config.getProperty("account"), config.getProperty("password"), verifyCode)
}