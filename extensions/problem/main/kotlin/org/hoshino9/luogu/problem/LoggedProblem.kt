package org.hoshino9.luogu.problem

import com.google.gson.JsonObject

interface ILoggedBaseProblem : IBaseProblem {
	val accepted: Boolean
}

open class LoggedBaseProblem(source: JsonObject) : BaseProblem(source), ILoggedBaseProblem {
	override val accepted: Boolean by delegate
}

interface ILoggedProblem : ILoggedBaseProblem, IProblem {
	val score: Int
	val showScore: Boolean
}

open class LoggedProblem(source: JsonObject) : Problem(source), ILoggedProblem {
	override val accepted: Boolean by delegate
	override val score: Int by delegate
	override val showScore: Boolean by delegate
}
