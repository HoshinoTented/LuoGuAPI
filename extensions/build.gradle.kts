import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
	kotlin("jvm")
}

group = "org.hoshino9"
version = "0.0.1"

dependencies {
	compile(project(":luogu"))
	compile("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.1.0")
}

tasks.withType<KotlinCompile> {
	kotlinOptions.jvmTarget = "1.8"
}