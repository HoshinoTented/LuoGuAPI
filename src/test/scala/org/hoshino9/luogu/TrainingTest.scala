package org.hoshino9.luogu

import org.hoshino9.luogu.training.TrainingList._
import org.junit.Test

class TrainingTest {
	@Test
	def testTrainings(): Unit = {
		val client = LuoGuClient()
		println(client.trainings().map(_.result).get)
	}

	@Test
	def testTraining(): Unit = {
		implicit val client = LuoGuClient()
		println(client.trainings().map(_.result.head.lift.get).get)
	}
}
