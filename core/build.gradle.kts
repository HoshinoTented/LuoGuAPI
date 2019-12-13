import org.gradle.api.internal.HasConvention
import org.jetbrains.kotlin.gradle.plugin.KotlinSourceSet

fun ktor(module: String, version: String): String = "io.ktor:ktor-$module:$version"
fun kotlinx(module: String, version: String) = "org.jetbrains.kotlinx:kotlinx-$module:$version"

val SourceSet.kotlin get() = (this as HasConvention).convention.getPlugin(KotlinSourceSet::class).kotlin

val ktorVersion: String by rootProject.extra
val coroutinesVersion: String by rootProject.extra

dependencies {
	api(kotlin("stdlib"))
	api(kotlin("reflect"))
	api(kotlin("script-runtime"))
	api(kotlinx("coroutines-core", coroutinesVersion))
	api(ktor("client-core", ktorVersion))
	api(ktor("client-core-jvm", ktorVersion))
	api(ktor("client-okhttp", ktorVersion))
	api(ktor("client-gson", ktorVersion))
	api("com.google.code.gson", "gson", "2.8.5")        // JSON parser
	api("com.squareup.okhttp3", "okhttp", "4.1.0")        // http library
	api("org.jsoup", "jsoup", "1.11.3")                // HTML parser
	testApi(kotlin("test-junit"))

//	val plugins = listOf(
//			"paintboard",
//			"photo",
//			"paste",
//			"problem",
//			"record").map {
//		":extensions:$it"
//	}.toTypedArray()
//
//	plugins.forEach {
//		testCompile(project(it))
//	}
}

val dependenciesJar = task<Jar>("dependenciesJar") {
	from(configurations.getByName("compile").map { if (it.isDirectory) it else zipTree(it) })
	archiveClassifier.set("dependencies")
}

artifacts {
	add("archives", dependenciesJar)
}
