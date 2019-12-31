package org.hoshino9.luogu.paintboard

import io.ktor.client.request.get
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.utils.emptyClient
import java.awt.image.BufferedImage

/**
 * 绘板数据类
 * 注意：Board 是竖直方向的
 *
 * @param height 高度
 * @param width 宽度
 */
data class Board(val height: Int, val width: Int) {
	val board: Array<Array<Int?>> = Array(width) {
		Array(height) {
			null as Int?
		}
	}

	operator fun get(pos: Pos): Int? {
		return board[pos.x][pos.y]
	}

	operator fun set(pos: Pos, color: Int?) {
		board[pos.x][pos.y] = color
	}
}

/**
 * 将绘板转化为 BufferedImage
 */
val Board.image: BufferedImage
	get() {
		val image = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

		board.forEachIndexed { x, line ->
			line.forEachIndexed inner@{ y, color ->
				image.setRGB(x, y, colors[color ?: return@inner].toRGB)
			}
		}

		return image
	}