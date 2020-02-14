import org.hoshino9.luogu.*
import java.io.File
import java.nio.file.Paths
import java.util.Properties

suspend fun main() {
	val properties = Properties().apply {
		load(File(rootPath).resolve("user.properties").inputStream())
	}

	val account: String by properties
	val password: String by properties

	val lg = LuoGu()
	val tmpdir = Paths.get(System.getProperty("java.io.tmpdir")).resolve("verify.png")

	tmpdir.toFile()
			.outputStream()
			.write(lg.verifyCode())

	println("Verify code was sent to your temp directory: ${tmpdir.toAbsolutePath()}")

	val code = readLine() !!

	lg.login(account, password, code)

	println("Cookies: ${lg.clientId.value}")
}
