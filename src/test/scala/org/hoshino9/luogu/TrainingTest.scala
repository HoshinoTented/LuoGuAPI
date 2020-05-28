package org.hoshino9.luogu

import com.google.gson.Gson
import org.hoshino9.luogu.training.TrainingList._
import org.junit.Test

class TrainingTest {
	@Test
	def testTrainings(): Unit = {
		val client = LuoGuClient()
		println(client.trainings().result)
	}

	@Test
	def testTraining(): Unit = {
		implicit val client = LuoGuClient()
		println(client.trainings().result.get(0).lift)
	}
}
