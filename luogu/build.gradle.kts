import org.gradle.api.internal.HasConvention
import org.jetbrains.kotlin.gradle.plugin.KotlinSourceSet

val SourceSet.kotlin get() = (this as HasConvention).convention.getPlugin(KotlinSourceSet::class).kotlin

dependencies {
	compile(kotlin("stdlib"))
	compile(kotlin("script-runtime"))
	compile("io.arrow-kt:arrow-core-data:0.9.0")
	compile("com.google.code.gson", "gson", "2.8.5")
	compile("com.squareup.okhttp3", "okhttp", "4.1.0")
	compile("org.jsoup", "jsoup", "1.11.3")
	testCompile(kotlin("test-junit"))

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

	plugins.forEach {
		testCompile(project(it))
	}
}

val dependenciesJar = task<Jar>("dependenciesJar") {
	from(configurations.getByName("compile").map { if (it.isDirectory) it else zipTree(it) })
	archiveClassifier.set("dependencies")
}

artifacts {
	add("archives", dependenciesJar)
}
