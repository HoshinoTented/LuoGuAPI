import org.hoshino9.luogu.contest.ContestListPage
import org.hoshino9.luogu.contest.contestListPage
import org.hoshino9.luogu.test.BaseTest
import org.junit.Test

class ContestTest : BaseTest() {
	@Test
	fun contestList() {
		luogu.contestListPage().let {
			println(it.count)

			it.contests.forEach {
				println("${it.id}: ${it.name}")
			}
		}
	}
}