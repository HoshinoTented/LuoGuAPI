plugins {
	maven
	kotlin("jvm") version "1.3.40"
}

val isCI: Boolean = System.getenv("CI").isNullOrBlank().not()

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
}