import org.hoshino9.luogu.problem.Problem
import org.hoshino9.luogu.problem.problemList
import org.hoshino9.luogu.test.BaseTest
import org.junit.Test

class ProblemTest : BaseTest() {
	@Test
	fun problemList() {
		luogu.problemList().result.map { Problem.Factory(it, luogu.client) }.forEach {
			println("${it.title}(${it.pid})[${it.difficulty}] ${it.tags} (${it.totalAccepted} / ${it.totalSubmit})")
		}
	}
}