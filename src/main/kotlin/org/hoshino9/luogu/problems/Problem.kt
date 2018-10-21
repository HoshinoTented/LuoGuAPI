package org.hoshino9.luogu.problems

data class Problem(
		val name : String,
		val background : String,
		val description : String,
		val demoTestCases : List<Pair<String, String>>,
		val tips : String,
		val tags : List<Nothing>,            //TODO
		val demoProgram : Solution            //Language should be AUTO
) {
}