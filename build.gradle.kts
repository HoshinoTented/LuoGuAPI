import org.jetbrains.kotlin.gradle.plugin.KotlinSourceSet

plugins {
	kotlin("jvm") version "1.3.10"
}

group = "org.hoshino9"
version = "0.0.1"

repositories {
	jcenter()
}

allprojects {
	apply {
		plugin("kotlin")
	}

	repositories {
		jcenter()
	}

	if (this.path != ":luogu") {
		sourceSets {
			val names = listOf("main", "test")

			names.forEach { name ->
				getByName(name) {
					withConvention(KotlinSourceSet::class) {
						kotlin.srcDir("$name/kotlin")

						resources.srcDir("$name/resources")
					}
				}
			}
		}

		dependencies {
			compile(project(":luogu"))
		}
	}
}