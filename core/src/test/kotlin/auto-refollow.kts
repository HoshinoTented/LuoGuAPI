import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.user.currentUser

val luogu = LuoGu("your client id", "your uid")
val currentUser = luogu.currentUser

tailrec fun refollow(page: Int) {
	val list = currentUser.followers(page).list
	if (list.isEmpty()) return

	list.forEach {
		currentUser.user.doFollow(it)
	}

	refollow(page + 1)
}

refollow(1)