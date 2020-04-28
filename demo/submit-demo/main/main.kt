import io.ktor.http.cio.websocket.Frame
import io.ktor.http.cio.websocket.readText
import org.hoshino9.luogu.LuoGuClient
import org.hoshino9.luogu.problem.ProblemPageBuilder
import org.hoshino9.luogu.record.Solution
import org.hoshino9.luogu.record.postSolution
import org.hoshino9.luogu.test.BaseTest

suspend fun main() {
	val lg = object : BaseTest() {}.luogu
	val problem = ProblemPageBuilder("P1001", LuoGuClient()).build().problem

	println(problem)

	val sol = Solution(problem.pid, Solution.Language.Haskell.ordinal, """
main :: IO ()
main = sum <$> map read <$> words <$> getLine >>= print
	""".trim(), false)

	lg.postSolution(sol).listen(lg) {
		for (frame in incoming) {
			frame as Frame.Text

			val content = frame.readText()
			if ("heartbeat" in content) break

			println(content)
		}
	}
}