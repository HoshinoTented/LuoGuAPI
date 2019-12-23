import io.ktor.client.features.ClientRequestException
import io.ktor.client.features.cookies.cookies
import io.ktor.client.features.websocket.webSocket
import io.ktor.client.features.websocket.ws
import io.ktor.http.HttpMethod
import io.ktor.http.cio.websocket.Frame
import io.ktor.http.cio.websocket.readBytes
import io.ktor.http.cio.websocket.readText
import io.ktor.http.cio.websocket.send
import io.ktor.http.cookies
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.record.Record
import org.hoshino9.luogu.record.Solution
import org.hoshino9.luogu.record.postSolution
import org.hoshino9.luogu.test.BaseTest
import org.hoshino9.luogu.utils.strData
import org.junit.Test

class RecordTest : BaseTest() {
	@Test
	fun record() {
		runBlocking {
			val job = launch {
				try {
					luogu.postSolution(Solution("P1001", Solution.Language.Haskell.ordinal, "main = putStrLn \"Hello world!\"")).listen(luogu) {
						while (true) {
							val frame = incoming.receive()

							if (frame is Frame.Text) {
								println(frame.readText())
							}
						}
					}
				} catch (e: ClientRequestException) {
					System.err.println(e.response.strData)
				}
			}

			job.join()
		}
	}
}