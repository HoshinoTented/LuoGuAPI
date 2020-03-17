import org.jetbrains.kotlin.gradle.plugin.KotlinSourceSet

fun ktor(module: String, version: String): String = "io.ktor:ktor-$module:$version"
fun kotlinx(module: String, version: String) = "org.jetbrains.kotlinx:kotlinx-$module:$version"

val ktorVersion: String by rootProject.extra
val coroutinesVersion: String by rootProject.extra

sourceSets {
	main.configure {
		withConvention(KotlinSourceSet::class) {
			kotlin.srcDirs("src/main/gen")
		}
	}
}

repositories {
	maven("https://dl.bintray.com/kotlin/ktor/")
}

dependencies {
	// kotlin
	api(kotlin("stdlib"))
	api(kotlin("reflect"))
	api(kotlin("script-runtime"))

	// kotlinx
	api(kotlinx("coroutines-core", coroutinesVersion))

	// ktor
	api(ktor("client-core", ktorVersion))
	api(ktor("client-core-jvm", ktorVersion))
	api(ktor("client-okhttp", ktorVersion))
	api(ktor("client-websockets", ktorVersion))
	api(ktor("client-gson", ktorVersion))

	// others
	api("org.jsoup", "jsoup", "1.11.3")                // HTML parser
	api("ch.qos.logback:logback-classic:1.2.1")

	// testing
	testApi(kotlin("test-junit"))
}

val createTestConfig = task("createTestConfig") {
	doLast {
		file("src/main/gen/TestConfig.kt").apply {
			if (exists().not()) {
				parentFile.mkdirs()
				createNewFile()
			}
		}.writeText("const val rootPath = \"${project.rootDir.absolutePath.replace("\\", "\\\\")}\"")
	}
}

tasks["compileKotlin"].dependsOn(createTestConfig)