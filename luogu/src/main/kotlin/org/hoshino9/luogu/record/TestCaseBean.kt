package org.hoshino9.luogu.record

data class TestCaseBean(
		override val description : String,
		override val exitCode : Int,
		override val status : TestCase.Status,
		override val subTask : Int,
		override val memory : Int,
		override val score : Int,
		override val signal : Int,
		override val time : Int
) : AbstractTestCase() {
	override lateinit var name : String
}