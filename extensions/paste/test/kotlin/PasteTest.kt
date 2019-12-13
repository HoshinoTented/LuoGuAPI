import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.paste.deletePaste
import org.hoshino9.luogu.paste.newPaste
import org.hoshino9.luogu.paste.pasteList
import org.hoshino9.luogu.test.BaseTest
import org.hoshino9.luogu.test.printAllMember
import org.junit.Test

class PasteTest : BaseTest() {
	@Test
	fun postPaste() {
		runBlocking {
			user.newPaste("qwq")
		}
	}

	@Test
	fun pasteList() {
		runBlocking {
			user.pasteList().list.forEach {
				it.printAllMember()
			}
		}
	}

	@Test
	fun deleteAll() {
		runBlocking {
			while (true) {
				val list = user.pasteList().list

				if (list.isEmpty()) break else {
					list.forEach {
						user.deletePaste(it.id)
						println("Deleted: ${it.id}")
					}
				}
			}
		}
	}
}