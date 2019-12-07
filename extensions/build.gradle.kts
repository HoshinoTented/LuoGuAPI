import org.jetbrains.kotlin.gradle.plugin.KotlinSourceSet

allprojects {
	sourceSets {
		val names = listOf(main, test)

		names.forEach { src ->
			src.configure {
				withConvention(KotlinSourceSet::class) {
					kotlin.srcDir("$name/kotlin")

				}

				resources.srcDir("$name/resources")
			}
		}
	}

	dependencies {
		compileOnly(project(":core"))
		testCompile(project(":core"))
		testCompile(kotlin("test-junit"))
	}
}