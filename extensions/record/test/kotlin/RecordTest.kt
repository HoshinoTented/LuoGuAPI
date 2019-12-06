import org.hoshino9.luogu.record.Solution
import org.hoshino9.luogu.record.postSolution
import org.hoshino9.luogu.test.BaseTest
import org.junit.Test

class RecordTest : BaseTest() {
	@Test
	fun record() {
		user.postSolution(Solution("P1001", Solution.Language.Haskell, "main = putStrLn \"Hello world!\"")).listen(luogu) { socket, msg ->
			println(msg)
		}
	}
}