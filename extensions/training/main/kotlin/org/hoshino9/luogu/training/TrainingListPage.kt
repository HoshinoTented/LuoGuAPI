package org.hoshino9.luogu.training

import com.google.gson.JsonArray
import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonElement
import com.google.gson.annotations.JsonAdapter
import io.ktor.client.request.get
import org.hoshino9.luogu.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.page.ListPage
import org.hoshino9.luogu.page.PageBuilder
import org.hoshino9.luogu.page.currentData
import org.hoshino9.luogu.utils.*
import java.lang.reflect.Type

@JsonAdapter(TrainingListPageImpl.Serializer::class)
interface TrainingListPage : ListPage {
	val result: List<BaseTraining>
}

data class TrainingListPageImpl(override val result: List<BaseTraining>, override val count: Int, override val perPage: Int) : TrainingListPage {
	companion object Serializer : JsonDeserializable<TrainingListPage>(TrainingListPage::class) {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): TrainingListPage {
			val trainings = json.asJsonObject["trainings"].asJsonObject.delegate
			val perPage: Int by trainings
			val count: Int by trainings
			val result: JsonArray by trainings

			val realResult = result.map {
				BaseTrainingImpl(it)
			}

			return TrainingListPageImpl(realResult, count, perPage)
		}
	}
}

class TrainingListPageBuilder(val page: Int = 1, val type: Type, client: HttpClient = emptyClient)
	: AbstractLuoGuPage(client), PageBuilder<TrainingListPage> {
	enum class Type {
		Official,
		Select,
	}

	override val url: String = "$baseUrl/training/list?type=${type.name.toLowerCase()}&page=$page"
	override fun build(): TrainingListPage {
		return TrainingListPageImpl(currentData)
	}
}