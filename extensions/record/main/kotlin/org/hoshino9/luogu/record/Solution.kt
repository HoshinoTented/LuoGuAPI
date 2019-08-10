package org.hoshino9.luogu.record

data class Solution(val pid : String, val language : Language, val code : String, val enableO2 : Boolean = false) {
	enum class Language {
		Auto,
		Pascal,
		C,
		Cpp,
		Cpp11,
		Python2,
		Python3,
		Java8,
		NodeJs,
		Shell,
		Cpp14,
		Cpp17,
		Ruby,
		Go,
		Rust,
		PHP7,
		CS,
		VB,
		Haskell,
		KotlinNative,
		KotlinJVM,
		Scala,
		Perl5,
		PyPy2,
		PyPy3,
	}
}