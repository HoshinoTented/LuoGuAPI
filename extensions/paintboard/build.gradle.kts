import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

dependencies {
	compile("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.2.0")
}

tasks.withType<KotlinCompile> {
	kotlinOptions.jvmTarget = "1.8"
}