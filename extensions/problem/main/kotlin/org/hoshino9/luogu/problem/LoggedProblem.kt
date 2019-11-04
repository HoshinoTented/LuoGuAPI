package org.hoshino9.luogu.problem

import com.google.gson.JsonObject

interface LoggedProblem : Problem {
	open class Factory(source: JsonObject) : Problem.Factory(source), LoggedProblem {
		override val isAccepted: Boolean by delegate
		override val score: Int by delegate
		override val showScore: Boolean by delegate
	}

	val isAccepted: Boolean
	val score: Int
	val showScore: Boolean
}

