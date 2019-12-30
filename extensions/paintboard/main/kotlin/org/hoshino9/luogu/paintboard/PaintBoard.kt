package org.hoshino9.luogu.paintboard

import io.ktor.client.request.get
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.utils.emptyClient
import java.awt.image.BufferedImage

data class Board(val height: Int, val width: Int) {
	val board: Array<Array<Int?>> = Array(height) {
		Array(width) {
			null as Int?
		}
	}

	val image: BufferedImage
		get() {
			val image = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

			board.forEachIndexed { x, line ->
				line.forEachIndexed { y, color ->
					image.setRGB(y, x, colors[color ?: return@forEachIndexed].toRGB)
				}
			}

			return image
		}

	operator fun get(pos: Pos): Int? {
		return board[pos.x][pos.y]
	}

	operator fun set(pos: Pos, color: Int?) {
		board[pos.x][pos.y] = color
	}
}

data class PaintBoard(val board: Board) {
	override fun equals(other: Any?): Boolean {
		return this === other
	}

	override fun hashCode(): Int {
		return board.hashCode()
	}
}

suspend fun paintBoard(): PaintBoard {
	val lines = emptyClient.get<String>("$baseUrl/paintBoard/board").lines().dropLast(1)
	val board = Board(400, 800)

	lines.forEachIndexed { x, line ->
		line.forEachIndexed { y, color ->
			val index = color.toString().toInt(32)
			board.board[y][x] = index
		}
	}

	return PaintBoard(board)
}