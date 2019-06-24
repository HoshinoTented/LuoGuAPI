package org.hoshino9.luogu.problem.experimental

import org.hoshino9.luogu.problem.experimental.Problem

interface LoggedProblem : Problem {
	val isAccepted: Boolean
	val score: Int
	val showScore: Boolean
}