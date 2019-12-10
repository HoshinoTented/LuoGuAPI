import org.hoshino9.luogu.paste.deletePaste
import org.hoshino9.luogu.paste.newPaste
import org.hoshino9.luogu.paste.pasteList
import org.hoshino9.luogu.test.BaseTest
import org.hoshino9.luogu.test.printAllMember
import org.junit.Test

class PasteTest : BaseTest() {
	@Test
	fun postPaste() {
		user.newPaste("qwq")
	}

	@Test
	fun pasteList() {
		user.pasteList().list.forEach {
			it.printAllMember()
		}
	}

	@Test
	fun deleteAll() {
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