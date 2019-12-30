package org.hoshino9.luogu.paintboard

/**
 * 图片提供者
 *
 * [current] 提供当前绘画的相对坐标和颜色
 *
 * [next] 使提供者移动到下一个坐标
 *
 * 抽象出 PhotoProvider 可以支持各种绘画策略
 */
interface PhotoProvider {
	fun current(): Pair<Pos, Int?>
	fun next()
}

/**
 * 默认绘画策略：顺序式
 *
 * 会沿着垂直方向进行绘画
 *
 * @param photo 目标图片
 */
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