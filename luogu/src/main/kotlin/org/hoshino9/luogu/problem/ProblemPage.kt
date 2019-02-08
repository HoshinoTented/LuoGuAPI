package org.hoshino9.luogu.problem

import org.hoshino9.luogu.user.User

interface ProblemPage : Problem {
	val author : User
}