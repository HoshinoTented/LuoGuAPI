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
			luogu.newPaste("qwq")
		}
	}

	@Test
	fun pasteList() {
		runBlocking {
			luogu.pasteList().list.forEach {
				it.printAllMember()
			}
		}
	}

	@Test
	fun deleteAll() {
		runBlocking {
			while (true) {
				val list = luogu.pasteList().list

				if (list.isEmpty()) break else {
					list.forEach {
						luogu.deletePaste(it.id)
						println("Deleted: ${it.id}")
					}
				}
			}
		}
	}
}