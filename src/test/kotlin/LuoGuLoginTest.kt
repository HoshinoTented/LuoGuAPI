import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.problems.Solution
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
					it.code == 200
				}?.let { user ->
					user.postSolution(Solution("P1000", Solution.Language.Cpp11, "//中文的注释", false)).run(::println)
				}
			}
		}
	}
}