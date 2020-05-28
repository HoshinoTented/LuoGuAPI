plugins {
	scala
}

group = "org.example"
version = "1.0-SNAPSHOT"

repositories {
	maven("https://maven.aliyun.com/repository/public")
	jcenter()
}

dependencies {
	implementation("org.scala-lang:scala-library:2.13.2")
	implementation("org.scala-lang:scala-reflect:2.13.2")
	implementation("org.typelevel:cats-effect_2.13:2.1.3")
	implementation("com.squareup.okhttp3:okhttp:4.6.0")
	implementation("com.typesafe.play", "play-json_2.13", "2.9.0")

	testImplementation("org.scalatest:scalatest_2.11:3.0.0")
	testImplementation("junit:junit:4.12")
}