package org.hoshino9.luogu.user

import com.google.gson.JsonArray
import com.google.gson.JsonObject
import io.ktor.client.request.get
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.utils.*

class FollowList(val user: User, val page: Int, val type: Type) {
	enum class Type {
		Followings,
		Followers
	}

	private val url = "$baseUrl/fe/api/user/${type.name.toLowerCase()}?user=${user.uid}&page=$page"
	private val data: JsonObject = runBlocking { json(emptyClient.get(url)) }
	private val users = data["users"].asJsonObject
	private val result: JsonArray = users["result"].asJsonArray

	val count: Int get() = users["count"].asInt
	val list: List<IBaseUser>
		get() {
			return result.map {
				BaseUser(it.asJsonObject)
			}
		}
}