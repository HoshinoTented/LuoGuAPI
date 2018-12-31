@file:Suppress("unused")

import kotlinx.coroutines.*
import org.hoshino9.luogu.*
import org.hoshino9.luogu.utils.*
import java.awt.Color
import java.io.OutputStream
import java.awt.image.BufferedImage
import java.io.File
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

inline fun iterateMatrixIndexed(width : Int, height : Int, action : (Int, Int) -> Unit) {
	(0 until width).forEach { x ->
		(0 until height).forEach { y ->
			action(x, y)
		}
	}
}

inline fun <T> iterateMatrix(width : Int, height : Int, matrix : T, action : (T, Int, Int) -> Unit) {
	iterateMatrixIndexed(width, height) { x, y ->
		action(matrix, x, y)
	}
}

inline fun BufferedImage.iterate(action : (BufferedImage, Int, Int) -> Unit) = iterateMatrix(width, height, this, action)

/**
 * 画板绘画函数
 *
 * @param x 格子的横坐标(左上为0)
 * @param y 格子的纵坐标(左上为0)
 * @param color 颜色的代码(请自行 F12 查看 data-cid 属性 或 使用先进的 IDE 在上方的 [colorList] 预览颜色)
 */
fun LuoGu.draw(x : Int, y : Int, color : Int) : Pair<DrawStatus, String> {
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
			} to this.toString()
		}
	}
}

@Suppress("MemberVisibilityCanBePrivate", "CanBeParameter")
class PaintBoard(
		clients : List<LuoGu>,
		val timeLimit : (List<LuoGu>) -> Long = { 30 * 1000 },
		val targetBoardColor : (Int, Int) -> Int,
		val coroutineScope : CoroutineScope = GlobalScope
) {
	private var it = 0
	private var timer = coroutineScope.async { Unit }

	val clients = clients.toMutableList()
	val currentClient get() = clients[it].client

	private fun removeUser(msg : String) {
		println("Failed, removed user: ${clients[it].loggedUser}($msg)")

		clients.removeAt(it)
		if (it == clients.size) it = 0
	}

	fun draw(x : Int, y : Int, color : Int) = runBlocking {
		fun checkColor(x : Int, y : Int) : Boolean {
			return if (targetBoardColor(x, y) == color) {
				println("Skipped ($x, $y)")

				true
			} else false
		}

		if (checkColor(x, y).not()) {
			println("Try to draw ($x, $y)...")

			loop@ while (true) {
				println("Waiting...")
				timer.await()

				if (checkColor(x, y)) break@loop

				val status = clients[it].draw(x, y, color)
				when (status.first) {
					DrawStatus.SUCCESSFUL -> {
						timer = coroutineScope.async { delay(timeLimit(clients)) }
						break@loop
					}

					DrawStatus.FAILED -> {
						if (status.second != """{"data":"操作过于频繁","status":500}""") removeUser(status.second) else {
							println("Failed, try again...(${status.second})")
							timer = coroutineScope.async { delay(timeLimit(clients)) }
						}
					}

					DrawStatus.UNKNOWN -> removeUser(status.second)
				}
			}

			println("User ${clients[it].loggedUser} drew ${x to y} with color $color")

			++ it
			if (it == clients.size) it = 0
		}
	}

}

fun LuoGu.drawFromImage(beginX : Int, beginY : Int, image : BufferedImage) {
	PaintBoard(
			listOf(this),
			targetBoardColor = { x, y ->
				boardMatrix[x][y].toString().toInt(32)
			}
	).run {
		image.iterate { _, x, y ->
			draw(x + beginX, y + beginY,
					colorList.indexOfFirst { it.rgb == image.getRGB(x, y) }.takeIf { it != - 1 } ?: throw IllegalArgumentException("Invalid color: ${image.getRGB(x, y)}")
			)
		}
	}
}

fun LuoGu.drawFromImage(beginX : Int, beginY : Int, image : File) = drawFromImage(beginX, beginY, ImageIO.read(image))

/**
 * 将色块代码转化为图片
 * @param vertical 是否水平(色块代码的一行是否对应图片的一行)
 */
fun List<String>.image(vertical : Boolean) : BufferedImage {
	return BufferedImage(
			if (vertical) first().length else size,
			if (vertical) size else first().length,
			BufferedImage.TYPE_INT_RGB).also { image ->
		forEachIndexed { x, line ->
			line.forEachIndexed { y, char ->
				image.setRGB(if (vertical) y else x, if (vertical) x else y, colorList[char.toString().toInt(32)].rgb)
			}
		}
	}
}

val List<String>.image : BufferedImage
	get() {
		return image(true)
	}

val LuoGu.boardMatrix : List<String>
	get() {
		return executeGet("paintBoard/board") { resp ->
			resp.assert()
			resp.data !!.lines()
		}
	}

val LuoGu.board : BufferedImage
	get() {
		return boardMatrix.dropLast(1).image(false)
	}

/**
 * 获取画板图片
 *
 * @param out 输出的流
 */
fun LuoGu.board(out : OutputStream) {
	ImageIO.write(board, "png", out)
}

/**
 * 获取带框框的画板, 用于突出部分区域
 * @param square 区块 ((beginX, beginY), (endX, endY))
 * @param color 边框颜色
 */
fun LuoGu.boardWithSquare(square : Pair<Pair<Int, Int>, Pair<Int, Int>>, color : Int) : BufferedImage {
	return board.also { image ->
		val (begin, end) = square

		fun Pair<Int, Int>.outBounds() : Boolean = first < 0 || second < 0 || first >= image.width || second >= image.height

		fun drawVerticalLine(pos : Pair<Int, Int>, length : Int) {
			(0 until length).forEach {
				(pos.first + it to pos.second).let { p ->
					if (p.outBounds().not()) image.setRGB(p.first, p.second, color)
				}
			}
		}

		fun drawHorizontalLine(pos : Pair<Int, Int>, length : Int) {
			(0 until length).forEach {
				(pos.first to pos.second + it).let { p ->
					if (p.outBounds().not()) image.setRGB(p.first, p.second, color)
				}
			}
		}

		drawVerticalLine(begin.first - 1 to begin.second - 1, end.first - begin.first + 1)
		drawVerticalLine(begin.first to end.second, end.first - begin.first + 1)
		drawHorizontalLine(begin.first - 1 to begin.second, end.second - begin.second + 1)
		drawHorizontalLine(end.first to begin.second - 1, end.second - begin.second + 1)
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

/**
 * 转换图片内色块为相似色块
 */
fun transform(image : BufferedImage) {
	image.iterate { img, x, y ->
		img.setRGB(x, y, colorList[findSimilarColor(img.getRGB(x, y))].rgb)
	}
}

/**
 * 检查图片色块
 */
fun checkImage(image : BufferedImage) : Boolean {
	image.iterate { img, x, y ->
		val color = img.getRGB(x, y)

		if (colorList.firstOrNull { it.rgb != color } == null) return false
	}

	return true
}

/**
 * 将图片转换为色块代码
 *
 * @param image 图片
 * @param out 输出流
 */
fun transToIndex(image : BufferedImage, out : OutputStream) {
	out.use {
		(0 until image.height).forEach { y ->
			(0 until image.width).forEach { x ->
				it.write(findSimilarColor(image.getRGB(x, y)).toString(32).toByteArray())
			}

			if (y != image.height - 1) it.write("\n".toByteArray())
		}
	}
}

fun List<LuoGu>.drawFromRemote(url : String, regex : Regex) {
	var posList : MutableList<Triple<Int, Int, Int>> = arrayListOf()

	PaintBoard(
			this,
			timeLimit = { 30000L / this.size },
			targetBoardColor = { x, y ->
				first().boardMatrix[x][y].toString().toInt(32)
			}).run {
		while (true) {
			if (posList.isEmpty()) {
				println("Getting remote data...")
				currentClient.executeGet(url) { resp ->
					resp.assert()

					val content = resp.data !!

					println("[GET]: $content")

					posList = regex.findAll(content).toList().map {
						Triple(it.groupValues[1].toInt(), it.groupValues[2].toInt(), it.groupValues[3].toInt())
					}.toMutableList()
				}
			}

			posList.first().run {
				posList.removeAt(0)

				draw(first, second, third)
			}
		}
	}
}