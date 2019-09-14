import org.gradle.api.internal.HasConvention
import org.jetbrains.kotlin.gradle.plugin.KotlinSourceSet

plugins {
	maven
	kotlin("jvm") version "1.3.40" apply false
	java
	`maven-publish`
	id("com.jfrog.bintray") version "1.7.3"
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

subprojects {
	apply {
		plugin("maven")
		plugin("java")
		plugin("maven-publish")
		plugin("com.jfrog.bintray")
	}

	bintray {
		user = "ice1000"
		key = findProperty("key").toString()
		setConfigurations("archives")
		pkg.apply {
			name = rootProject.name
			repo = "ice1000"
			githubRepo = "HoshinoTented/LuoGuAPI"
			publicDownloadNumbers = true
			vcsUrl = "https://github.com/HoshinoTented/LuoGuAPI.git"
			version.apply {
				vcsTag = "${project.version}"
				name = vcsTag
				websiteUrl = "https://github.com/HoshinoTented/LuoGuAPI/releases/tag/$vcsTag"
			}
		}
	}

	publishing {
		(publications) {
			create<MavenPublication>("maven") {
				from(components["java"])
				groupId = project.group.toString()
				artifactId = "${rootProject.name}-${project.name}"
				version = project.version.toString()
				artifact(tasks["sourcesJar"])
				pom.withXml {
					val root = asNode()
					root.appendNode("description", "API of LuoGu website")
					root.appendNode("name", project.name)
					root.appendNode("url", "https://github.com/HoshinoTented/LuoGuAPI")
					root.children().last()
				}
			}
		}
	}
}