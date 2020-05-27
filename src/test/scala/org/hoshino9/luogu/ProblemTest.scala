package org.hoshino9.luogu

import org.junit.Test
import problem.ProblemList._

class ProblemTest {
	@Test
	def testList(): Unit = {
		val client = LuoGuClient()
		println(client.problems())
	}

	@Test
	def testDetail(): Unit = {
		implicit val client: LuoGuClient = LuoGuClient()
		println(client.problems().result.get(0).lift)
	}
}
