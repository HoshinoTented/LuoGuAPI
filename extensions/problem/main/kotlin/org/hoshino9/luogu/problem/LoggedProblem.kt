package org.hoshino9.luogu.problem

import com.google.gson.JsonObject

interface IBaseLoggedBaseProblem : IBaseProblem {
	val accepted: Boolean
}

interface ILoggedBaseProblem : IBaseLoggedBaseProblem {
	val submitted: Boolean
}

open class LoggedBaseProblem(source: JsonObject) : BaseProblem(source), ILoggedBaseProblem {
	override val accepted: Boolean by delegate
	override val submitted: Boolean by delegate
}

interface ILoggedProblem : IBaseLoggedBaseProblem, IProblem {
	val score: Int
	val showScore: Boolean
}

open class LoggedProblem(source: JsonObject) : Problem(source), ILoggedProblem {
	override val accepted: Boolean by delegate
	override val score: Int by delegate
	override val showScore: Boolean by delegate
}
