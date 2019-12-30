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
				if (it == ' ') null else {
					it.toInt() - '0'.toInt()
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

	val manager = PainterManager(PaintBoard(board), Pos(5, 10), boardProvider = ::paintBoard)

	cookies.keys.forEach {
		val painter = Painter(cookies[it].toString(), it.toString().toInt())

		manager.add(painter, 500)
	}

	runBlocking(manager.coroutineContext) {
		paintBoard()
		val job = manager.paint()

		job.join()
	}
}