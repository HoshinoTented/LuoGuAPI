import io.ktor.client.features.websocket.webSocket
import io.ktor.client.features.websocket.ws
import io.ktor.http.HttpMethod
import io.ktor.http.cio.websocket.Frame
import io.ktor.http.cio.websocket.readBytes
import io.ktor.http.cio.websocket.readText
import io.ktor.http.cio.websocket.send
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.record.Record
import org.hoshino9.luogu.record.Solution
import org.hoshino9.luogu.record.postSolution
import org.hoshino9.luogu.test.BaseTest
import org.junit.Test

class RecordTest : BaseTest() {
	companion object {
		@JvmStatic
		fun main(args: Array<String>) {
			runBlocking {
				RecordTest().run {
					luogu.client.ws(
							method = HttpMethod.Get,
							host = "ws.luogu.com.cn",
							path = "/ws"
					) {
						send(Record.message("28687883"))

						while (true) {
							val frame = incoming.receive()

							when (frame) {
								is Frame.Text -> println(frame.readText())
								else -> println(frame.readBytes().joinToString())
							}
						}
					}
				}
			}
		}
	}

	@Test
	fun record() {
		runBlocking {
			val job = launch {
				luogu.postSolution(Solution("P1001", Solution.Language.Haskell, "main = putStrLn \"Hello world!\"")).listen(luogu) { socket, msg ->
					println(msg)
				}
			}

			delay(10000L)
		}
	}
}