import org.jetbrains.kotlin.gradle.plugin.KotlinSourceSet
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
	kotlin("jvm")
}

group = "org.hoshino9"
version = "0.0.1"

sourceSets {
	getByName("main") {
		withConvention(KotlinSourceSet::class) {
			kotlin.srcDir("src")
		}

		resources.srcDir("resources")
	}
}

dependencies {
	compile(project(":luogu"))
}

tasks.withType<KotlinCompile> {
	kotlinOptions.jvmTarget = "1.8"
}