import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.test.BaseTest
import org.hoshino9.luogu.test.printAllMember
import org.hoshino9.luogu.training.*
import org.hoshino9.luogu.user.LoggedUserPage
import org.hoshino9.luogu.user.UserPage
import org.hoshino9.luogu.user.currentUser
import org.junit.Test

class TrainingTest : BaseTest() {
	@Test
	fun trainingList() {
		val page = TrainingListPageBuilder(1, TrainingListPageBuilder.Type.Official, luogu.client).build().apply {
			printAllMember()
		}

		val training = page.result.first().lift(luogu.client).apply {
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

	@Test
	fun userTrainingList() {
		runBlocking {
			LoggedUserPage(luogu.uid.value.toInt(), luogu).trainingList().printAllMember()
		}
	}
}