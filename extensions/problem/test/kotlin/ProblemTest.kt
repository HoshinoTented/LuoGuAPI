import org.hoshino9.luogu.problem.*
import org.hoshino9.luogu.test.BaseTest
import org.hoshino9.luogu.test.printAllMember
import org.junit.Test

class ProblemTest : BaseTest() {
	@Test
	fun problemList() {
		client.problemList().printAllMember()
	}

	@Test
	fun problemInfo() {
		ProblemPageBuilder("P1000", client).build().problem.printAllMember()
	}

	@Test
	fun solutions() {
		SolutionPageBuilder("P1000", client).build().printAllMember()
	}

//	@Test
//	fun mark() {
//		runBlocking {
//			luogu.unmark("P3695")
//		}
//	}
}