package org.hoshino9.luogu.paintboard

interface PhotoProvider {
	fun current(): Pair<Pos, Int?>
	fun next()
}

class DefaultPhotoProvider(val photo: PaintBoard) : PhotoProvider {
	private var offset = Pos(0, 0)
	private val currentColor: Int?
		get() {
			return photo.board[offset]
		}

	private fun nextPos() {
		offset = Pos(offset.x + 1, offset.y)

		if (photo.board.height == offset.x) {
			offset = Pos(0, offset.y + 1)
		}

		if (photo.board.width == offset.y) {
			offset = Pos(0, 0)
		}
	}

	override fun current(): Pair<Pos, Int?> {
		return offset to currentColor
	}

	@Synchronized
	override fun next() {
		nextPos()
	}
}