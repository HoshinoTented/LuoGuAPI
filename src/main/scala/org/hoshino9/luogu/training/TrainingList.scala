package org.hoshino9.luogu.training

import com.google.gson.{Gson, JsonObject}
import com.google.gson.annotations.JsonAdapter
import org.hoshino9.luogu.baseUrl
import org.hoshino9.luogu.LuoGuClient
import org.hoshino9.luogu.json.{JavaList, Redirect}
import org.hoshino9.luogu.page.{ListPage, LuoGuClientPage}
import org.hoshino9.luogu.training.TrainingList.TrainingType.Type

@JsonAdapter(classOf[TrainingList.Redirection])
trait TrainingList extends ListPage {
	val result: JavaList[TrainingBase]
}

object TrainingList {

	private[luogu] class Redirection extends Redirect[TrainingList, Default]

	object TrainingType extends Enumeration {
		type Type = Value
		val official, select = Value
	}

	case class Default(result: JavaList[TrainingBase],
	                   count: Int,
	                   perPage: Int) extends TrainingList

	private class TrainingListPage(val `type`: Type, val page: Int, override val client: LuoGuClient) extends LuoGuClientPage {
		override val url: String = s"$baseUrl/training/list?type=${`type`.toString}&page=$page"

		def trainings: JsonObject = {
			currentData.getAsJsonObject("trainings")
		}
	}

	implicit class RichLuoGuClient(val client: LuoGuClient) extends AnyVal {
		def trainings(`type`: Type = TrainingType.official, page: Int = 1): TrainingList = {
			val trainings = new TrainingListPage(`type`, page, client).trainings

			new Gson().fromJson(trainings, classOf[TrainingList])
		}
	}

}
