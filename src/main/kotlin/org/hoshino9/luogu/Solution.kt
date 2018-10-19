package org.hoshino9.luogu

data class Solution(val pid : String, val language : Language, val code : String, val enableO2 : Boolean = false) {
	enum class Language(val value : Int) {
		Auto(0),
		Pascal(1),
		C(2),
		Cpp(3),
		Cpp11(4),
		Python2(6),
		Python3(7),
		Java8(8),
		NodeJs(9),
		Cpp14(11),
		Cpp17(12),
		Ruby(13),
		Go(14),
		Rust(15),
		PHP7(16),
		CS(17),		// Mono
		VB(18)		// Mono
	}						//What is the mean of mono
}