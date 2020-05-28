package org.hoshino9.luogu

import play.api.libs.json.JsResult

package object problem {
	type Difficulty = Int
	type ProblemID = String
	type ProblemType = String
	type ProblemTag = Int

	implicit class RichProblem(val problem: Problem) {
		def lift(implicit client: LuoGuClient): JsResult[ProblemDetail] = {
			import ProblemDetail._

			client.problem(problem.pid)
		}
	}

}
