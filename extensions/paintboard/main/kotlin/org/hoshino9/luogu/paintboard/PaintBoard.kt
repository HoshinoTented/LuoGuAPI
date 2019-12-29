package org.hoshino9.luogu.paintboard

data class Board(val height: Int, val weight: Int) {
	val board: Array<Array<Color?>> = Array(height) {
		Array(weight) {
			null as Color?
		}
	}

	operator fun get(pos: Pos): Color? {
		return board[pos.x][pos.y]
	}

	operator fun set(pos: Pos, color: Color?) {
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