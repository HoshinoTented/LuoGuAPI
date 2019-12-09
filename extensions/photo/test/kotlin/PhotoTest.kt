import okhttp3.MediaType.Companion.toMediaType
import org.hoshino9.luogu.photo.pushPhoto
import org.hoshino9.luogu.test.BaseTest
import org.junit.Test
import java.io.File
import java.nio.file.Paths

class PhotoTest : BaseTest() {
	companion object {
		@JvmStatic
		fun main(args: Array<String>) {
			PhotoTest().run {
				val clz = PhotoTest::class.java

				luogu.verifyCode(File("./test/resources/verify.png").outputStream())

				print("Please input verify code: ")

				val code = readLine() !!

				user.pushPhoto(
						photo = PhotoTest::class.java.getResource("nya.png").toURI().run(::File),
						verifyCode = code,
						contentType = "image/png".toMediaType()
				).run(::println)
			}
		}
	}
}