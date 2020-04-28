package org.hoshino9.luogu.problem

import com.google.gson.*
import com.google.gson.annotations.JsonAdapter
import org.hoshino9.luogu.LuoGuClient
import org.hoshino9.luogu.baseUrl
import org.hoshino9.luogu.page.*
import org.hoshino9.luogu.utils.Deserializable
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.delegate
import org.hoshino9.luogu.utils.emptyClient
import java.lang.reflect.Type

@JsonAdapter(ProblemListPageImpl.Serializer::class)
interface ProblemListPage : ListPage {
	companion object;

	val result: List<BaseProblem>
}

data class ProblemListPageImpl(override val result: List<BaseProblem>, override val count: Int, override val perPage: Int) : ProblemListPage {
	companion object Serializer : Deserializable<ProblemListPage>(ProblemListPage::class), JsonDeserializer<ProblemListPage> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): ProblemListPage = run {
			val data = json.asJsonObject
			val problems = data["problems"].asJsonObject.delegate
			val result: JsonArray by problems
			val count: Int by problems
			val perPage: Int by problems
			val realResult: List<BaseProblem> = result.map {
				BaseProblemImpl(it)
			}

			ProblemListPageImpl(realResult, count, perPage)
		}
	}
}

class ProblemListPageBuilder(val page: Int, val filter: ProblemSearchConfig, client: HttpClient = emptyClient) : AbstractLuoGuPage(client), PageBuilder<ProblemListPage> {
	override val url: String get() = "$baseUrl/problem/list?page=$page&$filter"

	override fun build(): ProblemListPage = run {
		ProblemListPageImpl(currentData)
	}
}

class NewProblemListPageBuilder(val page: Int, val filter: ProblemSearchConfig, client: LuoGuClient) : AbstractLuoGuClientPage(client), PageBuilder<ProblemListPage> {
	override val url: String
		get() = "$baseUrl/problem/list?page=$page&$filter"

	override fun build(): ProblemListPage {
		return ProblemListPageImpl(currentData)
	}
}