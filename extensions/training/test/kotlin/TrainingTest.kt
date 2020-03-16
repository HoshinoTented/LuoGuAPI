import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.test.BaseTest
import org.hoshino9.luogu.test.printAllMember
import org.hoshino9.luogu.training.*
import org.junit.Test

class TrainingTest : BaseTest() {
	@Test
	fun trainingList() {
		val page = TrainingListPage(1, TrainingListPage.Type.Official, luogu.client).apply {
			printAllMember()
		}

		val training = page.trainings.first().lift(page.client).apply {
			printAllMember()
		}
	}

	@Test
	fun newTraining() {
		runBlocking {
			val id = luogu.newTraining(TrainingForm.PersonalPublic("123", "456"))
			println(id)

			luogu.editTrainingProblems(id, listOf("P1001", "P1002"))
		}
	}
}