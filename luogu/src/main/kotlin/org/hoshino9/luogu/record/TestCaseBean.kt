package org.hoshino9.luogu.record

import com.google.gson.annotations.SerializedName

data class TestCaseBean(
		@SerializedName("desc") override val description : String,
		@SerializedName("exit_code") override val exitCode : Int,
		@SerializedName("flag") override val status : TestCase.Status,
		@SerializedName("subtask") override val subTask : Int,
		override val memory : Int,
		override val score : Int,
		override val signal : Int,
		override val time : Int
) : AbstractTestCase() {
	override lateinit var name : String
}