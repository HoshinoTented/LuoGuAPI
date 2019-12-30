import kotlinx.coroutines.delay
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.paintboard.*
import java.io.File
import java.util.Properties

val photo =
		"""
   1   
  121  
 12321 
1233321
 12321 
  121  
   1   
""".lines().filter { it.isNotEmpty() }.map {
			it.map {
				when (it.toInt() - '0'.toInt()) {
					1 -> Color.Red
					2 -> Color.Green
					3 -> Color.Blue

					else -> null
				}
			}
		}

fun main() {
	val cookies = Properties().apply {
		load(File("test/resources/cookies.properties").inputStream())
	}

	val board = Board(7, 7).apply {
		photo.forEachIndexed { x, line ->
			line.forEachIndexed { y, color ->
				this.board[x][y] = color
			}
		}
	}

	val manager = PainterManager(PaintBoard(board), Pos(5, 10)) {
		PaintBoard(Board(100, 100))
	}

	cookies.keys.forEach {
		val painter = Painter(it.toString(), cookies[it].toString().toInt())

		manager.add(painter, 500)
	}

	runBlocking(manager.coroutineContext) {
		val job = manager.paint()

		job.join()
	}
}