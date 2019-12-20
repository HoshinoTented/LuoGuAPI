import io.ktor.client.features.ClientRequestException
import io.ktor.util.toByteArray
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.contest.ContestListPage
import org.hoshino9.luogu.contest.contestListPage
import org.hoshino9.luogu.contest.contestPage
import org.hoshino9.luogu.contest.joinContest
import org.hoshino9.luogu.test.BaseTest
import org.hoshino9.luogu.test.printAllMember
import org.hoshino9.luogu.utils.strData
import org.junit.Test
import kotlin.reflect.full.memberProperties

class ContestTest : BaseTest() {
	@Test
	fun contestList() {
		luogu.contestListPage().run {
			printAllMember()
		}
	}

	@Test
	fun contestInfo() {
		luogu.contestListPage().contests.first().id.let {
			luogu.contestPage(it).run {
				printAllMember()
			}
		}
	}

//	@Test
//	fun contestJoin() {
//		runBlocking {
//			try {
//				luogu.joinContest(24975, "123")
//			} catch(e: ClientRequestException) {
//				e.response.strData.run(::println)
//			}
//		}
//	}
}