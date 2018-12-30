import org.hoshino9.luogu.*
import org.hoshino9.luogu.utils.*
import java.awt.Color
import java.io.OutputStream
import java.awt.image.BufferedImage
import java.io.InputStream
import javax.imageio.ImageIO

enum class DrawStatus {
	SUCCESSFUL,
	FAILED,
	UNKNOWN
}

val colorList = arrayOf(
		Color(0, 0, 0),
		Color(255, 255, 255),
		Color(170, 170, 170),
		Color(85, 85, 85),
		Color(254, 211, 199),
		Color(255, 196, 206),
		Color(250, 172, 142),
		Color(255, 139, 131),
		Color(244, 67, 54),
		Color(233, 30, 99),
		Color(226, 102, 158),
		Color(156, 39, 176),
		Color(103, 58, 183),
		Color(63, 81, 181),
		Color(0, 70, 112),
		Color(5, 113, 151),
		Color(33, 150, 243),
		Color(0, 188, 212),
		Color(59, 229, 219),
		Color(151, 253, 220),
		Color(22, 115, 0),
		Color(55, 169, 60),
		Color(137, 230, 66),
		Color(215, 255, 7),
		Color(255, 246, 209),
		Color(248, 203, 140),
		Color(255, 235, 59),
		Color(255, 193, 7),
		Color(255, 152, 0),
		Color(255, 87, 34),
		Color(184, 63, 39),
		Color(121, 85, 72)
)

/**
 * 画板绘画函数
 *
 * @param x 格子的横坐标(左上为0)
 * @param y 格子的纵坐标(左上为0)
 * @param color 颜色的代码(请自行 F12 查看 data-cid 属性 或 使用先进的IDE在上方的 colorList 预览颜色)
 */
fun LuoGu.draw(x : Int, y : Int, color : Int) : DrawStatus {
	return executePost("paintBoard/paint",
			listOf(
					"x" to x,
					"y" to y,
					"color" to color
			).params(), referer("paintBoard")) { resp ->
		resp.assert()

		json (resp.data!!) {
			when (this["status"]) {
				200 -> DrawStatus.SUCCESSFUL
				500 -> DrawStatus.FAILED

				else -> DrawStatus.UNKNOWN
			}
		}
	}
}

/**
 * 从图片中读取像素并绘画
 *
 * @receiver 用户(客户端)列表
 * @param beginX 开始的 x 坐标
 * @param beginY 开始的 y 坐标
 * @param input 图片输入流
 * @param timeLimit 自己看签名谢谢
 */
fun List<LuoGu>.drawFromImage(beginX : Int, beginY : Int, input : InputStream, timeLimit : (List<LuoGu>) -> Long = { 30 * 1000 }) {
	val clients = toMutableList()
	var it = 0

	val image = ImageIO.read(input)

	(0 until image.width).forEach { x ->
		(0 until image.height).forEach { y ->
			val color = colorList.indexOfFirst { it.rgb == image.getRGB(x, y) }.takeIf { it != -1 }
					?: throw IllegalArgumentException("Invalid color: ${image.getRGB(x, y).run(::Color)}")

			loop@ while (true) {
				when (clients[it].draw(x + beginX, y + beginY, color)) {
					DrawStatus.SUCCESSFUL -> break@loop
					DrawStatus.FAILED -> continue@loop
					DrawStatus.UNKNOWN -> {
						println("Removed user: ${clients[it].loggedUser}")

						clients.removeAt(it)
						if (it == clients.size) it = 0
					}
				}
			}

			println("User ${clients[it].loggedUser} drew ${x + beginX to y + beginY} with color $color")

			++ it
			if (it == clients.size) it = 0

			Thread.sleep(timeLimit(clients))
		}
	}
}

fun LuoGu.drawFromImage(beginX : Int, beginY : Int, input : InputStream, timeLimit : (List<LuoGu>) -> Long = { 30 * 1000}) = listOf(this).drawFromImage(beginX, beginY, input, timeLimit)

/**
 * 获取画板图片
 *
 * @param out 输出的流
 */
fun LuoGu.board(out : OutputStream) {
	executeGet("paintBoard/board") { resp ->
		resp.assert()

		resp.data?.let { board ->
			BufferedImage(800, 400, BufferedImage.TYPE_INT_RGB).also { image ->
				board.split('\n').forEachIndexed { x, line ->
					line.forEachIndexed { y, char ->
						image.setRGB(x, y, colorList[char.toString().toInt(32)].rgb)
					}
				}
			}.let { image ->
				ImageIO.write(image, "PNG", out)
			}
		}
	}
}