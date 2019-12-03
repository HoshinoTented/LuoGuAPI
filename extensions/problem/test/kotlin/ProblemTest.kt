import org.hoshino9.luogu.problem.BaseProblem
import org.hoshino9.luogu.problem.Problem
import org.hoshino9.luogu.problem.ProblemFactory
import org.hoshino9.luogu.problem.problemList
import org.hoshino9.luogu.test.BaseTest
import org.junit.Test

class ProblemTest : BaseTest() {
	@Test
	fun problemList() {
		luogu.problemList().result.map { ProblemFactory.newInstance(it, luogu.client, ::BaseProblem) }.forEach {
			println("${it.title}(${it.pid})[${it.difficulty}] ${it.tags} (${it.totalAccepted} / ${it.totalSubmit})")
		}
	}
}