package org.hoshino9.luogu.problem

import okhttp3.OkHttpClient
import org.hoshino9.luogu.tag.LuoGuTag

open class ProblemFromId(override val id : String, val client : OkHttpClient) : AbstractProblem() {
	override val difficulty : Problem.Difficulty
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val name : String
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val passPercent : Pair<String, String>
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val tags : List<LuoGuTag>
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
}