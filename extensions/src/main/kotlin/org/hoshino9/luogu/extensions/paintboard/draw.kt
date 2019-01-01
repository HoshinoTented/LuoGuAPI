@file:Suppress("unused")

package org.hoshino9.luogu.extensions.paintboard

import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.utils.*
import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import java.io.OutputStream
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

fun List<PaintUser>.drawFromImage(beginX : Int, beginY : Int, image : BufferedImage) {
	AutoPainting(this).run {
		image.iterate { _, x, y ->
			var c : Throwable?

			do {
				c = try {
					draw(x + beginX, y + beginY,
							colorList.indexOfFirst { it.rgb == image.getRGB(x, y) }.takeIf { it != - 1 } ?: throw IllegalArgumentException("Invalid color: ${image.getRGB(x, y)}")
					)

					null
				} catch (e : Throwable) {
					e
				}
			} while(c != null)
		}
	}
}

fun PaintUser.drawFromImage(beginX : Int, beginY : Int, image : BufferedImage) {
	listOf(this).drawFromImage(beginX, beginY, image)
}

fun PaintUser.drawFromImage(beginX : Int, beginY : Int, image : File) = drawFromImage(beginX, beginY, ImageIO.read(image))

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

fun List<PaintUser>.drawFromRemote(url : String, regex : Regex) {
	var posList : MutableList<Triple<Int, Int, Int>> = arrayListOf()

	AutoPainting(this).run {
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