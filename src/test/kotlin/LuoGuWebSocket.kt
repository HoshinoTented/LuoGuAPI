import org.hoshino9.luogu.problems.Solution
import org.hoshino9.luogu.record.Record

fun main(args : Array<String>) {
	LuoGuTest().apply {
		loadCookie()
	}.luogu.apply {
		//		Record("14281901").listen(this) { _, msg ->
//			if (msg.type != "heartbeat") {
//				msg.recordStatus.apply {
//					println("memory / time: $memory / $time")
//					println("all test case:")
//					detail.testCases.forEach {
//						println("""${it.name}: $score""")
//					}
//				}
//			} else println("heart beat")
//		}

		loggedUser.postSolution(Solution("P1001", Solution.Language.Haskell, """
main :: IO ()
main = do
  [a, b] <- (map read . words) <${'$'}> getLine

  print (a + b)
""")).apply {
			println(this)
		}.listen(this) { _, msg ->
			println("QAQ")
			if (msg.type != "heartbeat") {
				msg.recordStatus.apply {
					println("memory / time: $memory / $time")
					println("all test case:")
					detail.testCases.forEach {
						println("""${it.name}: $score""")
					}
				}
			} else println("heart beat")
		}
	}
}