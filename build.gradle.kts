import org.gradle.api.internal.HasConvention
import org.jetbrains.kotlin.gradle.plugin.KotlinSourceSet

plugins {
	maven
	kotlin("jvm") version "1.3.40"
}


val isCI: Boolean = System.getenv("CI").isNullOrBlank().not()
val SourceSet.kotlin get() = (this as HasConvention).convention.getPlugin(KotlinSourceSet::class).kotlin

repositories {
	if (isCI) jcenter() else maven("http://maven.aliyun.com/nexus/content/groups/public/")
}

allprojects {
	apply {
		plugin("maven")
		plugin("kotlin")
	}

	group = "org.hoshino9"
	version = "0.0.2"

	repositories {
		if (isCI) jcenter() else maven("http://maven.aliyun.com/nexus/content/groups/public/")
	}

	val sourcesJar = task<Jar>("sourcesJar") {
		from(sourceSets.getByName("main").kotlin)
		archiveClassifier.set("sources")
	}

	artifacts {
		add("archives", sourcesJar)
	}
}