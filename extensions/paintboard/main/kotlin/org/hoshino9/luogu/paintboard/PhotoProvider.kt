package org.hoshino9.luogu.paintboard

interface PhotoProvider {
	fun current(): Pair<Pos, Int?>
	fun next()
}

class DefaultPhotoProvider(val photo: Board) : PhotoProvider {
	private var offset = Pos(0, 0)
	private val currentColor: Int?
		get() {
			return photo[offset]
		}

	private fun nextPos() {
		offset = Pos(offset.x + 1, offset.y)

		if (photo.height == offset.x) {
			offset = Pos(0, offset.y + 1)
		}

		if (photo.width == offset.y) {
			offset = Pos(0, 0)
		}
	}

	override fun current(): Pair<Pos, Int?> {
		return offset to currentColor
	}

	override fun next() {
		nextPos()
	}
}