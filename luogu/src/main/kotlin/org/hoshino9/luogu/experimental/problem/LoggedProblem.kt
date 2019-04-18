package org.hoshino9.luogu.experimental.problem

interface LoggedProblem : Problem {
	val isAccepted: Boolean
	val score: Int
	val showScore: Boolean
}