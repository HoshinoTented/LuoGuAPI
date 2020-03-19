import io.ktor.client.features.cookies.cookies
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.LuoGuClient
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