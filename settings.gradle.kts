include(":extensions", ":luogu")

val plugins = listOf(
		"comment",
		"discuss",
		"training",
		"paintboard",
		"photo",
		"paste",
		"problem",
		"record").map {
	":extensions:$it"
}.toTypedArray()

include(*plugins)