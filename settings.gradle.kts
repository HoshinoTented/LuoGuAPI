include(":extensions", ":luogu")

val plugins = listOf(
		"comment",
		"discuss",
		"training",
		"paintboard",
		"photo",
		"paste",
		"problem").map {
	":extensions:$it"
}.toTypedArray()

include(*plugins)