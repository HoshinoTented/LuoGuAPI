import io.ktor.http.ContentType
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.photo.photoList
import org.hoshino9.luogu.photo.pushPhoto
import org.hoshino9.luogu.test.BaseTest
import org.hoshino9.luogu.test.printAllMember
import org.junit.Test
import java.io.File
import java.nio.file.Paths

class PhotoTest : BaseTest() {
	companion object {
		@JvmStatic
		fun main(args: Array<String>) {
			runBlocking {
				PhotoTest().run {
					File("extensions/photo/test/resources/verify.png").outputStream().write(luogu.verifyCode())

					print("Please input verify code: ")

					val code = readLine() !!

					luogu.pushPhoto(
							photo = PhotoTest::class.java.getResource("photo.png").toURI().run(::File),
							verifyCode = code,
							contentType = ContentType.Image.PNG
					).run(::println)
				}
			}
		}
	}

	@Test
	fun photoList() {
		luogu.photoList(1).printAllMember()
	}
}