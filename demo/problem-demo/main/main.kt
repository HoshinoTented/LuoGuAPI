import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.LuoGuClient
import org.hoshino9.luogu.page.maxPageCount
import org.hoshino9.luogu.problem.BaseProblem
import org.hoshino9.luogu.problem.problemList

fun main() {
	val lg = LuoGuClient()
	val max = lg.problemList().maxPageCount
	val seq = sequence {
		(1..max).forEach {
			yieldAll(lg.problemList(it).result)
		}
	}

	seq.forEach {
		println(it.pid)
	}
}