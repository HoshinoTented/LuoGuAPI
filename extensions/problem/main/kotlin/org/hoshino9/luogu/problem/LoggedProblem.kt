package org.hoshino9.luogu.problem

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.JsonObject
import com.google.gson.annotations.JsonAdapter
import org.hoshino9.luogu.LuoGuClient
import org.hoshino9.luogu.page.currentData
import org.hoshino9.luogu.utils.Deserializable
import org.hoshino9.luogu.utils.delegate
import java.lang.reflect.Type

interface IBaseLoggedBaseProblem : BaseProblem {
	companion object;
	val accepted: Boolean
}

@JsonAdapter(LoggedBaseProblemImpl.Serializer::class)
interface LoggedBaseProblem : IBaseLoggedBaseProblem {
	companion object;
	val submitted: Boolean
}

data class LoggedBaseProblemImpl(override val accepted: Boolean, override val submitted: Boolean, private val baseProblem: BaseProblem) : BaseProblem by baseProblem, LoggedBaseProblem {
	companion object Serializer : Deserializable<LoggedBaseProblem>(LoggedBaseProblem::class), JsonDeserializer<LoggedBaseProblem> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): LoggedBaseProblem {
			val source = json.asJsonObject
			val delegate = source.delegate

			val accepted: Boolean by delegate
			val submitted: Boolean by delegate
			val baseProblem: BaseProblem = context.deserialize(json, BaseProblem::class.java)

			return LoggedBaseProblemImpl(accepted, submitted, baseProblem)
		}
	}
}

@JsonAdapter(LoggedProblemImpl.Serializer::class)
interface LoggedProblem : IBaseLoggedBaseProblem, Problem {
	companion object;
	val score: Int
	val showScore: Boolean
}

data class LoggedProblemImpl(override val accepted: Boolean, override val score: Int, override val showScore: Boolean, private val problem: Problem) : Problem by problem, LoggedProblem {
	companion object Serializer : Deserializable<LoggedProblem>(LoggedProblem::class), JsonDeserializer<LoggedProblem> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): LoggedProblem {
			val source = json.asJsonObject
			val delegate = source.delegate

			val accepted: Boolean by delegate
			val score: Int by delegate
			val showScore: Boolean by delegate
			val problem: Problem = context.deserialize(json, Problem::class.java)

			return LoggedProblemImpl(accepted, score, showScore, problem)
		}
	}
}

interface LoggedProblemPage : ProblemPage {
	override val problem: LoggedProblem
}

data class LoggedProblemPageImpl(override val problem: LoggedProblem) : LoggedProblemPage {
	companion object Serializer : Deserializable<LoggedProblemPage>(LoggedProblemPage::class), JsonDeserializer<LoggedProblemPage> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): LoggedProblemPage = run {
			val data = json.asJsonObject.delegate
			val problem: JsonObject by data
			val solutions: SolutionListPage by data

			LoggedProblemPageImpl(LoggedProblemImpl(problem))
		}
	}
}

class LoggedProblemPageBuilder(pid: String, client: LuoGuClient) : ProblemPageBuilder(pid, client) {
	override fun build(): LoggedProblemPage = run {
		LoggedProblemPageImpl(currentData)
	}
}