import org.jetbrains.kotlin.gradle.plugin.KotlinSourceSet

sourceSets {
	val names = listOf(main, test)

	names.forEach { name ->
		name {
			withConvention(KotlinSourceSet::class) {
				kotlin.srcDir("$name/kotlin")

				resources.srcDir("$name/resources")
			}
		}
	}
}

dependencies {
	compile(project(":luogu"))
}