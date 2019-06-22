import org.gradle.api.internal.HasConvention
import org.jetbrains.kotlin.gradle.plugin.KotlinSourceSet

plugins {
	maven
}

val SourceSet.kotlin get() = (this as HasConvention).convention.getPlugin(KotlinSourceSet::class).kotlin

dependencies {
	compile(kotlin("stdlib"))
	compile("com.squareup.okhttp3", "okhttp", "3.11.0")
	compile("org.json", "json", "20180813")
	compile("org.jsoup", "jsoup", "1.11.3")
	testCompile(kotlin("test-junit", "1.3.10"))

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

val sourcesJar = task<Jar>("sourcesJar") {
	from(sourceSets.getByName("main").kotlin)
	archiveClassifier.set("sources")
}

val dependenciesJar = task<Jar>("dependenciesJar") {
	from(configurations.getByName("compile").map { if (it.isDirectory) it else zipTree(it) })
	archiveClassifier.set("dependencies")
}

artifacts {
	operator fun String.invoke(obj : Any) = add(this, obj)
	"archives"(sourcesJar)
	"archives"(dependenciesJar)
}
