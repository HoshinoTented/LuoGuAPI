package org.hoshino9.luogu.color

import java.awt.Color

internal val classMap by lazy {
	mapOf(
			"ping" to LuoGuColor.Pink,
			"red" to LuoGuColor.Red,
			"yellow" to LuoGuColor.Yellow,
			"orange" to LuoGuColor.Orange,
			"purple" to LuoGuColor.Purple,
			"green" to LuoGuColor.Green,
			"bluedark" to LuoGuColor.BlueDark,
			"bluelight" to LuoGuColor.BlueLight,
			"black" to LuoGuColor.Black,
			"gray" to LuoGuColor.Gray,
			"brown" to LuoGuColor.Brown,
			"greendark" to LuoGuColor.GreenDark
	)
}

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
	return classMap[className.substring(6)] ?: throw NoSuchElementException(className)
}