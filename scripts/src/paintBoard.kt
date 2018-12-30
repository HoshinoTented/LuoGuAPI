import org.hoshino9.luogu.*
import org.hoshino9.luogu.utils.*
import java.awt.Color
import java.io.OutputStream
import java.awt.image.BufferedImage
import java.io.File
import java.nio.Buffer
import javax.imageio.ImageIO
import kotlin.math.abs

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
 * @param color 颜色的代码(请自行 F12 查看 data-cid 属性 或 使用先进的 IDE 在上方的 [colorList] 预览颜色)
 */
fun LuoGu.draw(x : Int, y : Int, color : Int) : DrawStatus {
	return executePost("paintBoard/paint",
			listOf(
					"x" to x,
					"y" to y,
					"color" to color
			).params(), referer("paintBoard")) { resp ->
		resp.assert()

		json(resp.data !!) {
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
 * @param width 图片宽
 * @param height 图片高
 * @param timeLimit 自己看类型签名谢谢
 * @param getColor 根据传入的 x 和 y 返回一个 [colorList] 的索引
 */
inline fun List<LuoGu>.draw(beginX : Int, beginY : Int, width : Int, height : Int, timeLimit : (List<LuoGu>) -> Long = { 30 * 1000 }, getColor : (Int, Int) -> Int) {
	val clients = toMutableList()
	var it = 0

	(0 until width).forEach { x ->
		(0 until height).forEach { y ->
			val color = getColor(x, y)

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

inline fun LuoGu.draw(beginX : Int, beginY : Int, width : Int, height : Int, timeLimit : (List<LuoGu>) -> Long = { 30 * 1000 }, getColor : (Int, Int) -> Int) = listOf(this).draw(beginX, beginY, width, height, timeLimit, getColor)

fun LuoGu.drawFromImage(beginX : Int, beginY : Int, image : BufferedImage) = draw(beginX, beginY, image.width, image.height) { x, y ->
	colorList.indexOfFirst { it.rgb == image.getRGB(x, y) }.takeIf { it != - 1 } ?: throw IllegalArgumentException("Invalid color: ${image.getRGB(x, y)}")
}

fun LuoGu.drawFromImage(beginX : Int, beginY : Int, image : File) = drawFromImage(beginX, beginY, ImageIO.read(image))

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

/**
 * # 寻找最相似色块
 * 原理就是找到 r g b 三者的差和最小的那个色块
 *
 * @param rgb 被寻找的色块
 * @return 最相似色块在 [colorList] 内的索引
 */
fun findSimilarColor(rgb : Int) : Int {
	fun diff(a : Color, b : Color) : Int {
		return abs(a.red - b.red) + abs(a.green - b.green) + abs(a.blue - b.blue)
	}

	val color = Color(rgb)
	var minDiff = 0 to diff(color, colorList[0])

	(1 until colorList.size).forEach { it ->
		val cur = colorList[it]
		val nowDiff = diff(color, cur)

		if (nowDiff < minDiff.second) minDiff = it to nowDiff
	}

	return minDiff.first
}