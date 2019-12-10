import org.hoshino9.luogu.contest.ContestListPage
import org.hoshino9.luogu.contest.contestListPage
import org.hoshino9.luogu.contest.contestPage
import org.hoshino9.luogu.test.BaseTest
import org.hoshino9.luogu.test.printAllMember
import org.junit.Test
import kotlin.reflect.full.memberProperties

class ContestTest : BaseTest() {
	@Test
	fun contestList() {
		luogu.contestListPage().let {
			it.printAllMember()
			it.contests.forEach { it.printAllMember() }
		}
	}

	@Test
	fun contestInfo() {
		luogu.contestListPage().contests.first().id.let {
			println(it)

			luogu.contestPage(it).run {
				printAllMember()
				contest.printAllMember()
			}
		}
	}
}