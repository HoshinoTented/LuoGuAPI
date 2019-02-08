package org.hoshino9.luogu.tag

import java.awt.Color

open class LuoGuTag(val text : String, val id : Int, val color : Color) {
	override fun toString() : String {
		return text
	}
}