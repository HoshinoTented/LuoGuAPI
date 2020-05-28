package org.hoshino9.luogu.training

import org.hoshino9.luogu.{LuoGuClient, baseUrl}
import org.hoshino9.luogu.page.{ListPage, LuoGuClientPage}
import org.hoshino9.luogu.training.TrainingList.TrainingType.Type
import play.api.libs.json.{JsObject, JsResult, Json, Reads}

trait TrainingList extends ListPage {
	val result: Seq[TrainingBase]
}

object TrainingList {
	implicit val reads: Reads[TrainingList] = Reads {
		Json.reads[Default].reads
	}

	object TrainingType extends Enumeration {
		type Type = Value
		val official, select = Value
	}

	case class Default(result: Seq[TrainingBase],
	                   count: Int,
	                   perPage: Int) extends TrainingList

	private class TrainingListPage(val `type`: Type, val page: Int, override val client: LuoGuClient) extends LuoGuClientPage {
		override val url: String = s"$baseUrl/training/list?type=${`type`.toString}&page=$page"

		def trainings: JsObject = {
			currentData("trainings").as[JsObject]
		}
	}

	implicit class RichLuoGuClient(val client: LuoGuClient) extends AnyVal {
		def trainings(`type`: Type = TrainingType.official, page: Int = 1): JsResult[TrainingList] = {
			val trainings = new TrainingListPage(`type`, page, client).trainings

			Json.fromJson(trainings)
		}
	}

}
