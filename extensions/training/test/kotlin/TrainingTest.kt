import org.hoshino9.luogu.test.BaseTest
import org.hoshino9.luogu.test.printAllMember
import org.hoshino9.luogu.training.TrainingListPage
import org.hoshino9.luogu.training.lift
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
}