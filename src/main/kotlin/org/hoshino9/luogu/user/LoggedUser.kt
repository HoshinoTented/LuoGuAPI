package org.hoshino9.luogu.user

import okhttp3.MediaType
import okhttp3.MultipartBody
import okhttp3.RequestBody
import org.hoshino9.luogu.*
import org.hoshino9.luogu.data.CodeObject
import org.hoshino9.luogu.data.StatusObject
import org.hoshino9.luogu.comment.Comment
import org.hoshino9.luogu.benben.BenBenType
import org.hoshino9.luogu.benben.BenbenUtils
import org.hoshino9.luogu.paste.DefaultPaste
import org.hoshino9.luogu.paste.Paste
import org.hoshino9.luogu.photo.Photo
import org.hoshino9.luogu.photo.PhotoUtils
import org.hoshino9.luogu.problem.Solution
import org.hoshino9.luogu.record.Record
import org.hoshino9.luogu.results.SignedInStatus
import org.hoshino9.luogu.utils.*
import org.json.JSONObject
import org.jsoup.Jsoup
import java.io.File
import java.lang.IllegalStateException

/**
 * **你谷**用户类
 * 但仅限于已登录的用户
 * 未登录的用户请用 `String` 代替
 * 等到 Kotlin1.3 可以改用 `inline class`
 */
@Suppress("MemberVisibilityCanBePrivate", "unused", "UNUSED_PARAMETER")
open class LoggedUser(val luogu : LuoGu, uid : String) : User(uid, luogu.client) {
	companion object {
		/**
		 * 实例化一个 LoggedUser 对象
		 * @param luogu 已经登陆过的洛谷客户端
		 * @return 返回一个 LoggedUser 对象
		 *
		 * @see LuoGuUtils.getUserIdFromPage
		 */
		@JvmName("newInstance")
		operator fun invoke(luogu : LuoGu) : LoggedUser {
			return LoggedUser(luogu, luogu.myuid)
		}
	}

	/**
	 * 获取签到状态信息
	 * @return 返回一个签到状态类
	 * @throws IllegalStateException 未签到时抛出
	 *
	 * @see SignedInStatus
	 */
	val signInStatus : SignedInStatus
		get() {
			val doc = luogu.homePage.run(Jsoup::parse)
			val node = doc.body().children()
					.getOrNull(1)?.children()
					?.getOrNull(1)?.children()
					?.getOrNull(1)?.children()
					?.first()?.children()
					?.first()?.children()
					?.first()?.children()
					?.first()?.children()
					?.getOrNull(1) ?: throw NoSuchElementException()
			return SignedInStatus(node.children())
		}

	override val spacePage : UserSpacePage by lazy { UserSpacePage(this, luogu.client) }

	/**
	 * **你谷**签到
	 */
	fun signIn() {
		return luogu.getExecute("index/ajax_punch") { resp ->
			resp.assert()
		}
	}

	/**
	 * 获取犇犇列表
	 * @param type 犇犇的类型
	 * @param page 页数, 默认为`1`
	 * @return 返回一个犇犇的列表
	 *
	 * @see Comment
	 * @see BenBenType
	 */
	@JvmOverloads
	fun getBenben(type : BenBenType, page : Int = 1) : List<Comment> {
		luogu.getExecute("feed/${type.toString().toLowerCase()}?page=$page") { resp ->
			resp.assert()
			val content = resp.data !!
			return BenbenUtils.getBenben(Jsoup.parse(content).body())
		}
	}

	/**
	 * 剪切板
	 * @param code `markdown` 代码
	 * @param public 是否公开, 默认 **true**
	 * @return 返回剪切板的代码
	 */
	@JvmOverloads
	fun postPaste(code : String, public : Boolean = true) : Paste {
		return luogu.postExecute("paste/post", mapOf(
				"content" to code,
				"verify" to "",
				"public" to if (public) "1" else "0"
		).params(), referer("paste")) { resp ->
			resp.assert()

			val content = resp.data !!
			val data = globalGson.fromJson(content, StatusObject::class.java)

			if (data.status != 200) throw IllegalAPIStatusCodeException(data.status, data.data ?: "")
			DefaultPaste(data.data !!, luogu.client)
		}
	}

	fun deletePaste(paste : Paste) {
		luogu.postExecute("paste/delete/${paste.id}", headers = referer("paste/${paste.id}")) { resp ->
			resp.assert()
		}
	}

	fun pasteList(page : Int = 1) : List<Paste> {
		val regex = Regex("""https://www.luogu.org/paste/(\w+)""")
		luogu.getExecute("paste?page=$page") { resp ->
			resp.assert()
			val content = resp.data !!

			return Jsoup.parse(content).toString().run { regex.findAll(this) }.map {
				DefaultPaste(it.groupValues[1], luogu.client)
			}.toList()
		}
	}

	/**
	 * 发射犇犇
	 * @param text 犇犇内容
	 */
	fun postBenben(text : String) {
		luogu.postExecute("api/feed/postBenben", mapOf("content" to text).params(), referer()) { resp ->
			resp.assert()
			val content = resp.data !!

			json(content) {
				val status = getInt("status")
				val data = get("data")
				if (status != 200) throw IllegalAPIStatusCodeException(status, data.toString())
			}
		}
	}

	/**
	 * 提交题解
	 * @param solution 题解对象
	 * @return 返回 Record 对象
	 *
	 * @see Solution
	 * @see Record
	 */
	fun postSolution(solution : Solution) : Record {
		return luogu.postExecute("api/problem/submit/${solution.pid}",
				mapOf(
						"code" to solution.code,
						"lang" to solution.language.value.toString(),
						"enableO2" to if (solution.enableO2) "1" else "0",
						"verify" to ""
				).params(), referer("problemnew/show/${solution.pid}")
		) { resp ->
			resp.assert()
			val content = resp.data !!

			json(content) {
				val status = optInt("status")
				val data = get("data")

				if (status == 200) {
					data as JSONObject
					Record(data.get("rid").toString())
				} else throw IllegalAPIStatusCodeException(status, data.toString())
			}
		}
	}

	/**
	 * 上传图片到**你谷**
	 * @param file 图片的 File 对象
	 * @throws IllegalAPIStatusCodeException 当 api 状态码不为 201 时抛出
	 * @throws IllegalStatusCodeException 当 请求状态码不为 200 时抛出
	 *
	 * @see File
	 */
	fun postPhoto(file : File) {
		luogu.postExecute("app/upload", MultipartBody.Builder()
				.setType(MultipartBody.FORM)
				.addFormDataPart("picupload", file.name, RequestBody.create(MediaType.parse("application/octet-stream"), file))
				.build(),
				referer("app/upload")) { resp ->
			resp.assert()
			val content = resp.data !!

			val data = globalGson.fromJson(content, CodeObject::class.java)

			if (data.code != 201) throw IllegalAPIStatusCodeException(data)
		}
	}

	/**
	 * 获取未阅读列表(就是右上角的新通知)
	 * @return 返回一个 Message 和 Notice 数量的Pair
	 */
	fun getUnread() : Pair<Int, Int> {
		return luogu.getExecute("space/ajax_getchatnum") { resp ->
			resp.assert()

			json(resp.data !!) {
				getInt("code").let { code ->
					if (code != 200) throw IllegalAPIStatusCodeException(code, getString("message"))
					getJSONObject("more").let { more ->
						more.getInt("messagenum") to more.getInt("noticenum")
					}
				}
			}
		}
	}

	/**
	 * 图床列表
	 * @return 返回一个图片的列表
	 *
	 * @see Photo
	 */
	fun photoList() : List<Photo> {
		luogu.getExecute("app/upload") { resp ->
			resp.assert()

			val page = resp.data !!
			return PhotoUtils.getPhotos(Jsoup.parse(page))
		}
	}

	/**
	 * 删除图片
	 * @param photo 需要删除的图片
	 */
	fun deletePhoto(photo : Photo) {
		photo.run {
			luogu.postExecute("app/upload?delete=1&uploadid=$id", headers = referer("app/upload")) {
				it.assert()
			}
		}
	}
}