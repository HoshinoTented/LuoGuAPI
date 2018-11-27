import org.gradle.api.internal.HasConvention
import org.jetbrains.kotlin.gradle.plugin.KotlinSourceSet
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
	kotlin("jvm") version "1.3.0"
	maven
}

group = "org.hoshino9"
version = "0.0.1"

val SourceSet.kotlin get() = (this as HasConvention).convention.getPlugin(KotlinSourceSet::class).kotlin

repositories {
	jcenter()
}

dependencies {
	compile(kotlin("stdlib"))
	compile(kotlin("script-util"))
	compile("com.squareup.okhttp3", "okhttp", "3.11.0")
	compile("org.json", "json", "20180813")
	compile("com.google.code.gson", "gson", "2.8.5")
	compile("org.jsoup", "jsoup", "1.11.3")
	testCompile(kotlin("test-junit"))
}

tasks.withType<KotlinCompile> {
	kotlinOptions.jvmTarget = "1.8"
}

val sourcesJar = task<Jar>("sourcesJar") {
	from(sourceSets.getByName("main").kotlin)
	classifier = "sources"
}

val dependenciesJar = task<Jar>("dependenciesJar") {
	from(configurations.compile.map { if (it.isDirectory) it else zipTree(it) })
	classifier = "all"
}

artifacts {
	operator fun String.invoke(obj : Any) = add(this, obj)
	"archives"(sourcesJar)
	"archives"(dependenciesJar)
}
