package org.hoshino9.luogu

import org.hoshino9.luogu.test.TestBase
import org.hoshino9.luogu.problem.solution.SolutionList._
import org.junit.Test

class SolutionTest extends TestBase {
	@Test
	def testSolutionList(): Unit = {
		println(client.solutionList("P1000").result)
	}
}
