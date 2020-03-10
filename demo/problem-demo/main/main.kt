import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.page.maxPageCount
import org.hoshino9.luogu.problem.BaseProblem
import org.hoshino9.luogu.problem.problemList

fun main() {
	val lg = LuoGu()
	val seq = sequence {
		var i = 1

		while (i <= lg.problemList().maxPageCount) {
			yieldAll(lg.problemList(i).result)
			i += 1
		}
	}

	seq.forEach {
		println(it.pid)
	}
}