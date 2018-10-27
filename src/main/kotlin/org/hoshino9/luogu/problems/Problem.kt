@file:Suppress("unused")

package org.hoshino9.luogu.problems

import org.hoshino9.luogu.problems.tags.LuoGuTag

open class Problem(open val pid : String) {
	abstract class Difficulty(text : String) : LuoGuTag(text, - 1)
	sealed class Red : Difficulty("入门难度")
	sealed class Orange : Difficulty("普及-")
	sealed class Yellow : Difficulty("普及/提高-")
	sealed class Green : Difficulty("普及+/提高")
	sealed class Blue : Difficulty("提高+/省选-")
	sealed class Purple : Difficulty("省选/NOI-")
	sealed class Black : Difficulty("NOI/NOI+/CTSC")

	open val difficulty : Difficulty get() = TODO()
	open val name : String get() = TODO()
	open val passPercent : Pair<Long, Long> get() = TODO()
	open val tags : List<LuoGuTag> get() = TODO()

	override fun toString() : String {
		return pid
	}
}