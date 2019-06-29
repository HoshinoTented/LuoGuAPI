package org.hoshino9.luogu.problem.experimental

import org.hoshino9.luogu.problem.experimental.Problem
import org.hoshino9.luogu.utils.HttpClient

interface LoggedProblem : Problem {
	open class Factory(pid: String, client: HttpClient) : Problem.Factory(pid, client), LoggedProblem {
		override val isAccepted: Boolean
			get() = data["isAccepted"].asBoolean

		override val score: Int
			get() = data["score"].asInt

		override val showScore: Boolean
			get() = data["showScore"].asBoolean
	}

	val isAccepted: Boolean
	val score: Int
	val showScore: Boolean
}

