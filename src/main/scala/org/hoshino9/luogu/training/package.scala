package org.hoshino9.luogu

import org.hoshino9.luogu.training.Training._

package object training {

	implicit class RichTrainingBase(val base: TrainingBase) extends AnyVal {
		def lift(implicit client: LuoGuClient): Training = {
			client.training(base.id)
		}
	}

}
