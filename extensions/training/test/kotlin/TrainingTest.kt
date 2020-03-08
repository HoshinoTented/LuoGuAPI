import org.hoshino9.luogu.test.BaseTest
import org.hoshino9.luogu.test.printAllMember
import org.hoshino9.luogu.training.TrainingListPage
import org.junit.Test

class TrainingTest : BaseTest() {
	@Test
	fun trainingList() {
		TrainingListPage(1, TrainingListPage.Type.Official, luogu.client).printAllMember()
	}
}