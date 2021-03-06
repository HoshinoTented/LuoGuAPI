package org.hoshino9.luogu.record

/**
 * 题解类
 *
 * @param pid 题目 ID
 * @param language 语言代码，可通过 [Solution.Language.ordinal] 获得
 * @param code 代码主体
 * @param enableO2 是否开启 O2
 */
data class Solution(val pid: String, val language: Int, val code: String, val enableO2: Boolean = false) {
	enum class Language(val fullName: String) {
		Auto("Auto Select"),
		Pascal("Pascal"),
		C("C"),
		Cpp("C++"),
		Cpp11("C++ 11"),
		SubmitAnswer("Submit Answer"),
		Python2("Python 2"),
		Python3("Python 3"),
		Java8("Java 8"),
		NodeJs("Node v8.9"),
		Shell("Shell"),
		Cpp14("C++ 14"),
		Cpp17("C++ 17"),
		Ruby("Ruby"),
		Go("Go"),
		Rust("Rust"),
		PHP7("PHP 7"),
		CS("C#"),
		VB("Visual Basic"),
		Haskell("Haskell"),
		KotlinNative("Kotlin/Native"),
		KotlinJVM("Kotlin/JVM"),
		Scala("Scala"),
		Perl("Perl"),
		PyPy2("PyPy 2"),
		PyPy3("PyPy 3"),
		WenYan("WenYan")
	}
}