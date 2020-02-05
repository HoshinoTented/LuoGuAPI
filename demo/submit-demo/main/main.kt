import io.ktor.http.cio.websocket.Frame
import io.ktor.http.cio.websocket.readBytes
import io.ktor.http.cio.websocket.readText
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.record.Record
import org.hoshino9.luogu.record.Solution
import org.hoshino9.luogu.record.postSolution

suspend fun main() {
	val lg = LuoGu("clientId", "uid".toInt())

	val sol = Solution("P1001", Solution.Language.Haskell.ordinal, """
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