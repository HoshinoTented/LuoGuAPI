import LuoGuTest.Companion.config
import LuoGuTest.Companion.verifyPath
import org.hoshino9.luogu.LuoGu
import java.nio.file.Files
import java.util.Scanner

suspend fun main() {
	LuoGuTest().run {
		login()
		println("logged in: $user")

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