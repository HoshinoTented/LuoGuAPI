import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.paintboard.*
import java.io.File
import java.util.Properties
import javax.imageio.ImageIO

const val paintDelay = 10010L

fun repl(manager: PainterManager) {
	while (true) {
		val command = readLine()?.takeIf { it.isNotBlank() }?.split(' ') ?: continue

		command.firstOrNull()?.let { cmd ->
			when (cmd) {
				"board" -> {
					val path = command.getOrNull(1)

					if (path == null) {
						println("need output path.")
					} else {

						println("Printing paint board...")

						runBlocking(Dispatchers.IO) {
							ImageIO.write(paintBoard().image, "png", File(path))
						}
					}
				}

				"add" -> {
					val clientId = command.getOrNull(1)
					val uid = command.getOrNull(2)?.toIntOrNull()

					if (clientId == null || uid == null) {
						println("client id or uid can not be null")
					} else {
						manager.add(Painter(clientId, uid), paintDelay)
						println("added $uid")
					}
				}

				"stop" -> {
					manager.job?.cancel()

					println("Stopping...")
					return
				}

				"timer" -> {
					manager.timers.joinToString(prefix = "[", postfix = "]") {
						it.painter.uid.toString()
					}.run(::println)
				}

				"queue" -> {
					manager.requestQueue.joinToString(prefix = "[", postfix = "]") {
						it.painter.uid.toString()
					}.run(::println)
				}

				else -> println("Unknown command: $cmd")
			}
		}
	}
}

fun main() {
	val cookies = Properties().apply {
		load(File("test/resources/cookies.properties").inputStream())
	}

	val photo = File("test/resources/photo.txt").readText().lines()

	val board = Board(7, 7).apply {
		photo.forEachIndexed { x, line ->
			line.forEachIndexed { y, color ->
				this.board[x][y] = if (color == ' ') null else color.toString().toInt(32)
			}
		}
	}

	val manager = PainterManager(DefaultPhotoProvider(board), Pos(0, 0), boardProvider = ::paintBoard)

	cookies.keys.forEach {
		val painter = Painter(cookies[it].toString(), it.toString().toInt())

		manager.add(painter, paintDelay)
	}

	manager.paint()

	repl(manager)
}