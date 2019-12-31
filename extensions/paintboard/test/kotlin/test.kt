import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.paintboard.*
import java.io.File
import java.util.Properties
import javax.imageio.ImageIO

const val paintDelay = 10010L

fun repl(manager: PainterManager) {
	while (true) {
		val commands = readLine()?.takeIf { it.isNotBlank() }?.split(' ') ?: return
		val command = commands.first()

		when (command) {
			"board" -> {
				println("Downloading...")

				runBlocking(Dispatchers.IO) {
					ImageIO.write(manager.boardProvider.board().image, "png", File("test/resources/board.png"))
				}
			}

			else -> println("Unknown command: $command")
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

	val manager = PainterManager(DefaultPhotoProvider(board), Pos(100, 100), boardProvider = WebSocketBoardProvider(logger = null))

	cookies.keys.forEach {
		val painter = Painter(LuoGu(cookies[it].toString(), it.toString().toInt()).client, it.toString().toInt())

		manager.add(painter, paintDelay)
	}

	manager.paint()

	repl(manager)
}