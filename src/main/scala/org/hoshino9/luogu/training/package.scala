package org.hoshino9.luogu

import org.hoshino9.luogu.training.Training._
import play.api.libs.json.JsResult

package object training {

	implicit class RichTrainingBase(val base: TrainingBase) extends AnyVal {
		def lift(implicit client: LuoGuClient): JsResult[Training] = {
			client.training(base.id)
		}
	}

}
