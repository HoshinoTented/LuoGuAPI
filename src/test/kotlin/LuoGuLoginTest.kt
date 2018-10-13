import org.hoshino9.luogu.LuoGu
import java.io.FileOutputStream
import java.nio.file.Paths
import java.util.Properties
import java.util.Scanner

class LuoGuLoginTest {
	companion object {
		@JvmStatic
		fun main(args : Array<String>) {
			var account = ""
			var password = ""

			Properties().apply {
				load(LuoGuLoginTest::class.java.getResourceAsStream("user.properties"))
			}.run {
				account = this.getProperty("account")
				password = this.getProperty("password")
			}

			LuoGu().run {
				verifyCode(Paths.get("src/test/resources/verify.png").toFile().run(::FileOutputStream))

				println("Please input verify code")
				val verifyCode : String = Scanner(System.`in`).next()

				login(account, password, verifyCode) {
					it.run(::println)
				}.let { user ->
					user.signIn().run(::println)
					println("Suppress Warning")
				}
			}
		}
	}
}