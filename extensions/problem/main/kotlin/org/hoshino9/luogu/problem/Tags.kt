package org.hoshino9.luogu.problem

enum class Difficulty(val content: String) {
	Unknown("暂无评定"),
	Red("入门"),
	Orange("普及-"),
	Yellow("普及/提高-"),
	Green("普及+/提高"),
	Blue("提高+/省选-"),
	Purple("省选/NOI-"),
	Black("NOI/NOI+/CTSC")
}

enum class Type(val id: String) {
	LuoGu("P"),
	CodeForces("CF"),
	AtCoder("AT"),
	SPOJ("SP"),
	UVA("UVA")
}