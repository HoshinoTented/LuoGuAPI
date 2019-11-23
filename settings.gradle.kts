include(":extensions", ":luogu")

val plugins = listOf(
		"paintboard",
		"photo",
		"paste",
		"problem",
		"record").map {
	":extensions:$it"
}.toTypedArray()

include(*plugins)

pluginManagement {
	repositories {
		if (System.getenv("CI").isNullOrBlank()) maven("https://maven.aliyun.com/repository/public")
		gradlePluginPortal()
	}
}