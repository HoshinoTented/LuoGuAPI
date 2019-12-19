package org.hoshino9.luogu.problem

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.JsonObject
import com.google.gson.annotations.JsonAdapter
import org.hoshino9.luogu.utils.Deserializable
import java.lang.reflect.Type

interface IBaseLoggedBaseProblem : IBaseProblem {
	val accepted: Boolean
}

@JsonAdapter(LoggedBaseProblem.Serializer::class)
interface ILoggedBaseProblem : IBaseLoggedBaseProblem {
	val submitted: Boolean
}

data class LoggedBaseProblem(override val accepted: Boolean, override val submitted: Boolean, private val baseProblem: IBaseProblem) : IBaseProblem by baseProblem, ILoggedBaseProblem {
	companion object Serializer : Deserializable<ILoggedBaseProblem>(ILoggedBaseProblem::class), JsonDeserializer<ILoggedBaseProblem> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): ILoggedBaseProblem {
			val source = json.asJsonObject

			val accepted: Boolean = source["accepted"].asBoolean
			val submitted: Boolean = source["submitted"].asBoolean
			val baseProblem: IBaseProblem = context.deserialize(json, IBaseProblem::class.java)

			return LoggedBaseProblem(accepted, submitted, baseProblem)
		}
	}
}

@JsonAdapter(LoggedProblem.Serializer::class)
interface ILoggedProblem : IBaseLoggedBaseProblem, IProblem {
	val score: Int
	val showScore: Boolean
}

data class LoggedProblem(override val accepted: Boolean, override val score: Int, override val showScore: Boolean, private val problem: IProblem) : IProblem by problem, ILoggedProblem {
	companion object Serializer : Deserializable<ILoggedProblem>(ILoggedProblem::class), JsonDeserializer<ILoggedProblem> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): ILoggedProblem {
			val source = json.asJsonObject

			val accepted: Boolean = source["accepted"].asBoolean
			val score: Int = source["score"].asInt
			val showScore: Boolean = source["showScore"].asBoolean
			val problem: IProblem = context.deserialize(json, IProblem::class.java)

			return LoggedProblem(accepted, score, showScore, problem)
		}
	}
}
