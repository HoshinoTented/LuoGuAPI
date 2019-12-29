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
	compile(kotlin("stdlib"))
	compile(kotlin("reflect"))
	compile(kotlin("script-runtime"))

	// kotlinx
	compile(kotlinx("coroutines-core", coroutinesVersion))

	// ktor
	compile(ktor("client-core", ktorVersion))
	compile(ktor("client-core-jvm", ktorVersion))
	compile(ktor("client-okhttp", ktorVersion))
	compile(ktor("client-websockets", ktorVersion))
	compile(ktor("client-gson", ktorVersion))

	// others
	compile("org.jsoup", "jsoup", "1.11.3")                // HTML parser

	// testing
	testCompile(kotlin("test-junit"))
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