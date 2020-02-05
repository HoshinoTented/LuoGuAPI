import org.hoshino9.luogu.*
import java.io.File
import java.nio.file.Paths
import java.util.Properties

suspend fun main() {
	val properties = Properties().apply {
		load(ClassLoader.getSystemResourceAsStream("user.properties"))
	}

	val username: String by properties
	val password: String by properties

	val lg = LuoGu()
	val tmpdir = Paths.get(System.getProperty("java.io.tmpdir")).resolve("verify.png")

	tmpdir.toFile()
			.outputStream()
			.write(lg.verifyCode())

	println("Verify code was sent to your temp directory: ${tmpdir.toAbsolutePath()}")

	val code = readLine() !!

	lg.login(username, password, code)

	println("Cookies: ${lg.clientId.value}")
}
