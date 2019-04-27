plugins {
	kotlin("jvm") version "1.3.30"
}

repositories {
	jcenter()
}

allprojects {
	apply {
		plugin("kotlin")
	}

	group = "org.hoshino9"
	version = "0.0.2"

	repositories {
		jcenter()
	}
}