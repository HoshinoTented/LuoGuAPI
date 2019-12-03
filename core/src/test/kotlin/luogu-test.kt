import LuoGuTest.Companion.config
import LuoGuTest.Companion.verifyPath
import org.hoshino9.luogu.LuoGu
import java.nio.file.Files
import java.util.Scanner

fun main() {
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

fun LuoGuTest.login() {
	luogu = LuoGu()
	luogu.verifyCode(Files.newOutputStream(verifyPath.also { path ->
		path.toFile().let {
			if (it.exists().not()) {
				it.parentFile.mkdirs()
				it.createNewFile()
			}
		}
	}))
	println("Please input verify code")
	val verifyCode: String = Scanner(System.`in`).next()
	luogu.login(config.getProperty("account"), config.getProperty("password"), verifyCode)
}