package org.hoshino9.luogu.user

import com.google.gson.*
import com.google.gson.annotations.JsonAdapter
import io.ktor.client.request.get
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.utils.*
import java.lang.reflect.Type

@JsonAdapter(FollowListUserImpl.Serializer::class)
interface FollowListUser : BaseUser {
	val blogAddress: String?
	val followingCount: Int
	val followerCount: Int
	val ranking: Int?
	val userRelationship: Int
	val reverseUserRelationship: Int
	val passedProblemCount: Int
	val submittedProblemCount: Int
}

data class FollowListUserImpl(override val blogAddress: String?, override val followingCount: Int, override val followerCount: Int, override val ranking: Int?, override val userRelationship: Int, override val reverseUserRelationship: Int, override val passedProblemCount: Int, override val submittedProblemCount: Int, val baseUser: BaseUser) : BaseUser by baseUser, FollowListUser {
	companion object Serializer : Deserializable<FollowListUser>(FollowListUser::class), JsonDeserializer<FollowListUser> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): FollowListUser {
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
			val baseUser: BaseUser = context.deserialize(json, BaseUser::class.java)

			return FollowListUserImpl(blogAddress, followingCount, followerCount, ranking, userRelationship, reverseUserRelationship, passedProblemCount, submittedProblemCount, baseUser)
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
	val list: List<FollowListUser>
		get() {
			return result.map {
				FollowListUserImpl(it)
			}
		}
}