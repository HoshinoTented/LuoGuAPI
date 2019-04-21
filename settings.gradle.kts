include(":extensions", ":luogu")

val plugins = listOf("paintboard").map {
	":extensions:$it"
}.toTypedArray()

include(*plugins)