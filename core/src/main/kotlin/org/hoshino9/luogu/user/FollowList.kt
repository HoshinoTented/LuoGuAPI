package org.hoshino9.luogu.user

import com.google.gson.*
import com.google.gson.annotations.JsonAdapter
import io.ktor.client.request.get
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.utils.*
import java.lang.reflect.Type

@JsonAdapter(FollowListUser.Serializer::class)
interface IFollowListUser : IBaseUser {
	val blogAddress: String?
	val followingCount: Int
	val followerCount: Int
	val ranking: Int?
	val userRelationship: Int
	val reverseUserRelationship: Int
	val passedProblemCount: Int
	val submittedProblemCount: Int
}

data class FollowListUser(override val blogAddress: String?, override val followingCount: Int, override val followerCount: Int, override val ranking: Int?, override val userRelationship: Int, override val reverseUserRelationship: Int, override val passedProblemCount: Int, override val submittedProblemCount: Int, val baseUser: IBaseUser) : IBaseUser by baseUser, IFollowListUser {
	companion object Serializer : Deserializable<IFollowListUser>(IFollowListUser::class), JsonDeserializer<IFollowListUser> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): IFollowListUser {
			val source = json.asJsonObject
			val delegate = source.delegate

			val blogAddress: String? by delegate
			val followingCount: Int by delegate
			val followerCount: Int by delegate
			val ranking: Int? by delegate
			val userRelationship: Int by delegate
			val reverseUserRelationship: Int by delegate
			val passedProblemCount: Int by delegate
			val submittedProblemCount: Int by delegate
			val baseUser: IBaseUser = context.deserialize(json, IBaseUser::class.java)

			return FollowListUser(blogAddress, followingCount, followerCount, ranking, userRelationship, reverseUserRelationship, passedProblemCount, submittedProblemCount, baseUser)
		}
	}
}

class FollowList(val uid: Int, val page: Int, val type: Type, val client: HttpClient = emptyClient) {
	enum class Type {
		Followings,
		Followers
	}

	private val url = "$baseUrl/fe/api/user/${type.name.toLowerCase()}?user=$uid&page=$page"
	private val data: JsonObject = runBlocking { json(client.get(url)) }
	private val delegate = data.delegate
	private val users: JsonObject by delegate
	private val usersDelegate = users.delegate
	private val result: JsonArray by usersDelegate

	val count: Int by usersDelegate
	val list: List<IFollowListUser>
		get() {
			return result.map {
				FollowListUser(it)
			}
		}
}