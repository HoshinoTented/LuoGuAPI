package org.hoshino9.luogu.extensions.paintboard

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.OutputStream
import kotlin.math.abs

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