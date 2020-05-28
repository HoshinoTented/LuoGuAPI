package org.hoshino9.luogu

import org.hoshino9.luogu.problem.{Problem, ProblemBase}
import org.junit.Test
import play.api.libs.json.Json
import problem.ProblemList._

class ProblemTest {
	@Test
	def testList(): Unit = {
		val client = LuoGuClient()
		println(client.problems().get)
	}

	@Test
	def testDetail(): Unit = {
		implicit val client: LuoGuClient = LuoGuClient()
		println(client.problems().map(_.result.head.lift.get).get)
	}
}
