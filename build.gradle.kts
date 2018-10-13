import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
	kotlin("jvm") version "1.2.71"
}

group = "org.hoshino9"
version = "0.0.1"

repositories {
	jcenter()
}

dependencies {
	compile(kotlin("stdlib-jdk8"))
	compile("org.apache.httpcomponents:httpclient:jar:4.5.6")
}

tasks.withType<KotlinCompile> {
	kotlinOptions.jvmTarget = "1.8"
}