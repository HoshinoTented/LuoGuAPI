import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.user.currentUser
import org.hoshino9.luogu.user.doFollow

val luogu = LuoGu("your client id", "your uid")
val currentUser = luogu.currentUser

tailrec suspend fun refollow(page: Int) {
	val list = currentUser.followers(page).list
	if (list.isEmpty()) return

	list.forEach {
		luogu.doFollow(it)
	}

	refollow(page + 1)
}

runBlocking {
	refollow(1)
}