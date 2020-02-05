import org.jetbrains.kotlin.gradle.plugin.KotlinSourceSet

allprojects {
	sourceSets {
		main {
			withConvention(KotlinSourceSet::class) {
				kotlin.srcDir("main")
			}

			resources.srcDir("resources")
		}
	}

	dependencies {
		implementation(project(":core"))
	}
}