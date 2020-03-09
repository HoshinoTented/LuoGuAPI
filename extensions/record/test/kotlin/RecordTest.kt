import io.ktor.client.features.ClientRequestException
import io.ktor.http.cio.websocket.Frame
import io.ktor.http.cio.websocket.readText
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.record.Record
import org.hoshino9.luogu.test.BaseTest
import org.hoshino9.luogu.utils.strData
import org.junit.Test

class RecordTest : BaseTest() {
	@Test
	fun record() {
		runBlocking {
			val job = launch {
				try {
					// luogu.postSolution(Solution("P1001", Solution.Language.Haskell.ordinal, "main = putStrLn \"Hello world!\""))
					Record("28862889").listen(luogu) {
						for (frame in incoming) {
							if (frame is Frame.Text) {
								val text = frame.readText()

								if ("heartbeat" in text) break

								println(text)
							}
						}
					}
				} catch (e: ClientRequestException) {
					System.err.println(e.response.strData())
				}
			}

			job.join()
		}
	}
}