import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.problem.ProblemPage
import org.hoshino9.luogu.problem.mark
import org.hoshino9.luogu.problem.problemList
import org.hoshino9.luogu.problem.unmark
import org.hoshino9.luogu.test.BaseTest
import org.hoshino9.luogu.test.printAllMember
import org.junit.Test

class ProblemTest : BaseTest() {
	@Test
	fun problemList() {
		luogu.problemList().result.map {
			it.printAllMember()
		}
	}

	@Test
	fun problemInfo() {
		ProblemPage("P1000").printAllMember()
	}

//	@Test
//	fun mark() {
//		runBlocking {
//			luogu.unmark("P3695")
//		}
//	}
}