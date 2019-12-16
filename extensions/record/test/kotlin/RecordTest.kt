import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.record.Solution
import org.hoshino9.luogu.record.postSolution
import org.hoshino9.luogu.test.BaseTest
import org.junit.Test

class RecordTest : BaseTest() {
	@Test
	fun record() {
		runBlocking {
			val job = launch {
				luogu.postSolution(Solution("P1001", Solution.Language.Haskell, "main = putStrLn \"Hello world!\"")).listen(luogu) { socket, msg ->
					println(msg)
				}
			}

			job.join()
		}
	}
}