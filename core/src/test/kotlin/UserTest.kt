import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.test.BaseTest
import org.hoshino9.luogu.test.printAllMember
import org.hoshino9.luogu.user.currentUser
import org.hoshino9.luogu.user.doFollow
import org.junit.Test

class UserTest : BaseTest() {
	@Test
	fun user() {
		luogu.currentUser.run {
			printAllMember()
			followers().printAllMember()
			followings().printAllMember()
		}
	}
}