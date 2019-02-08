package org.hoshino9.luogu.problem

import org.jsoup.select.Elements

data class DefaultProblemContent constructor(
		override val id : String,
		override val background : Elements,
		override val description : Elements,
		override val inAndOutFormat : Elements,
		override val inAndOut : Elements,
		override val tip : Elements
) : AbstractProblemContent()