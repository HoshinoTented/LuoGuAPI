@file:Suppress("MemberVisibilityCanBePrivate", "CanBeParameter", "unused")

package org.hoshino9.luogu.paintboard

import org.json.JSONArray
import java.awt.image.BufferedImage

data class ColorPos(val x : Int, val y : Int, val color : Int)

internal val emptyColorPos = ColorPos(-1, -1, -1)

interface DrawScheme : Iterable<ColorPos>

class ImageDrawScheme(val img : BufferedImage) : DrawScheme {
	class ColorPosIterator(val img : BufferedImage) : Iterator<ColorPos> {
		private var itX = 0
		private var itY = 0

		override fun hasNext() : Boolean {
			return itX < img.width
		}

		override fun next() : ColorPos {
			return ColorPos(itX, itY, img.getRGB(itX, itY)).apply {
				++ itY
				if (itY == img.height) {
					++ itX
					itY = 0
				}
			}
		}
	}

	override fun iterator() : Iterator<ColorPos> {
		return ColorPosIterator(img)
	}
}

class PositionListDrawScheme(val posList : JSONArray) : DrawScheme {
	class ColorPosIterator(val it : Iterator<Any>) : Iterator<ColorPos> {
		override fun hasNext() : Boolean {
			return it.hasNext()
		}

		override fun next() : ColorPos {
			return (it.next() as JSONArray).let { pos ->
				ColorPos(pos[0] as Int, pos[1] as Int, pos[2] as Int)
			}
		}
	}

	override fun iterator() : Iterator<ColorPos> {
		return ColorPosIterator(posList.iterator())
	}
}