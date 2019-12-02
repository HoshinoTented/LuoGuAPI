package org.hoshino9.luogu.tag

import java.awt.Color

open class ColoredLuoGuTag(val text: String, id: Int, val color: Color) : IdLuoGuTag(id) {
	override fun toString(): String {
		return text
	}
}