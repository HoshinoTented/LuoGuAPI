package org.hoshino9.luogu.color

import org.jsoup.nodes.Element
import java.awt.Color

enum class LuoGuColor(val color : String) {
	Pink("F495A0"),
	Red("E74C3C"),
	Yellow("F1C40F"),
	Orange("E67E22"),
	Purple("8E44AD"),
	Green("5Eb95E"),
	BlueDark("2E468C"),
	BlueLight("3498DB"),
	Black("34495E"),
	Gray("BBBBBB"),
	Brown("996600"),
	GreenDark("C9054310");

	fun toColor() : Color {
		return Color(color.toInt(16), color.length != 6)
	}
}

fun colorFromClass(className : String) : LuoGuColor {
	return LuoGuColor.values().firstOrNull { it.name.equals(className.substring(6), true) }
			?: throw NoSuchElementException(className)
}

val Element.luoguColor : LuoGuColor?
	get() {
		return classNames().firstOrNull { it.startsWith("lg-bg-") }?.run(::colorFromClass)
	}