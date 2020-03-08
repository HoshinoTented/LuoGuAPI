rootProject.name = "luoguapi"

include(":extensions", ":core", ":demo")

val plugins = listOf(
		"contest"
		, "paintboard"
		, "photo"
		, "paste"
		, "problem"
		, "record"
		, "training"
).map {
	":extensions:$it"
}.toTypedArray()

val demos = listOf(
		"core",
		"submit",
		"problem"
).map {
	":demo:$it-demo"
}.toTypedArray()

include(*plugins)
include(*demos)

pluginManagement {
	repositories {
		if (System.getenv("CI").isNullOrBlank()) maven("https://maven.aliyun.com/repository/public")
		gradlePluginPortal()
	}
}