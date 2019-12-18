import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.user.currentUser
import org.hoshino9.luogu.user.doFollow

val luogu = LuoGu("your client id", "your uid".toInt())
val currentUser = luogu.currentUser

tailrec suspend fun refollow(page: Int) {
	val list = currentUser.followers(page).list
	if (list.isEmpty()) return

	list.forEach {
		if (it.userRelationship == 1) println("Followed ${it.uid}") else {
			println("Following ${it.uid}")
			luogu.doFollow(it.uid)
		}
	}

	refollow(page + 1)
}

runBlocking {
	refollow(1)
}