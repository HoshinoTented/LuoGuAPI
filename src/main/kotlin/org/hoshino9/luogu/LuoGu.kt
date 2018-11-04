@file:Suppress("unused", "UNUSED_PARAMETER")

package org.hoshino9.luogu

import org.apache.http.HttpEntity
import org.apache.http.client.HttpClient
import org.apache.http.client.methods.HttpGet
import org.apache.http.util.EntityUtils
import org.hoshino9.luogu.benben.BenBenType
import org.hoshino9.luogu.benben.LuoGuComment
import org.hoshino9.luogu.photo.LuoGuPhoto
import org.hoshino9.luogu.photo.ParsedLuoGuPhoto
import org.hoshino9.luogu.practice.PracticeBlock
import org.hoshino9.luogu.problems.*
import org.json.JSONObject
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.hoshino9.luogu.record.Record
import java.io.OutputStream

/**
 * # LuoGU
 * **你谷**客户端类
 */
@Suppress("MemberVisibilityCanBePrivate")
open class LuoGu @JvmOverloads constructor(val client : HttpClient = defaultClient) : HttpClient by client {
	companion object Companion {        //先写个 Companion 吧, 以后可能要 rename
		const val baseUrl = "https://www.luogu.org"

		fun user(url : String) : LuoGuUser {
			return url.substring(url.lastIndexOf('=') + 1).run(::LuoGuUser)
		}

		/**
		 * 解析主站滚动图片
		 * @param page 主站页面
		 * @return 返回一个列表, 其中 Pair 的 first 是链接, second 是图片
		 *
		 * @see Document
		 */
		fun sliderPhotos(page : Document) : List<Pair<String, String>> {
			val name = "lg-slider"
			return page.getElementById(name)?.run {
				children().first()?.run {
					children().filter {
						it.className() != "clone"
					}.mapNotNull {
						val linkElement = it.children().first() ?: return@mapNotNull null
						val imgElement = linkElement.children().first() ?: return@mapNotNull null
						val link = linkElement.attr("href")
						val img = imgElement.attr("src")

						link to img
					}
				} ?: throw NoSuchElementException("first child of $name")
			} ?: throw NoSuchElementException(name)
		}

		/**
		 * 获取 user
		 * @param document Document对象, 即**你谷**主站页面, 因为**你谷**某些奇怪的原因, 而没有开放API, 所以只能从网页中爬了
		 * @return 返回一个uid, **Nullable**
		 *
		 * @see Document
		 */
		fun userId(document : Document) : String? {
			return (document.body()
					.getElementsByAttribute("myuid")
					.first()
					?.attr("myuid") ?: "").takeIf { it != "" }
		}

		/**
		 * **TODO**
		 * 因为**你谷**不让别人爬评测记录, 所以就不能爬了
		 * 获取评测记录
		 * @param page Document对象, 即**你谷**主站页面
		 * @param filter 用于过滤的函数
		 * @return 返回一个评测记录列表
		 *
		 * @see Document
		 * @see Record
		 */
		inline fun records(page : Document, filter : (Record) -> Boolean = { true }) : List<Record> {
			TODO("""评测记录相关页面不欢迎一切爬虫行为。
我相信如果你正在制作爬虫，一定能够看到本段文字。
请勿再制作任何爬取评测记录的爬虫。""")
		}

		/**
		 * 解析benben
		 * @param list
		 * @return 返回一个犇犇列表
		 *
		 * @see LuoGuComment
		 */
		fun benben(list : Element) : List<LuoGuComment> {
			return list.children().mapNotNull {
				if (it.tagName() == "li") LuoGuComment(it) else null
			}
		}

		/**
		 * 一个奇怪的Token, 似乎十分重要, 大部分操作都需要这个
		 * @param page 任意一个**你谷**页面
		 * @return 返回 `csrf-token`, 若找不到则返回 **null**
		 */
		fun csrfToken(page : Document) : String? {
			return page.head().getElementsByTag("meta").firstOrNull { it?.attr("name") == "csrf-token" }?.attr("content")
		}

		fun photo(list : Element) : List<LuoGuPhoto> {
			return list.getElementsByClass("lg-table-row").map {
				ParsedLuoGuPhoto(it)
			}
		}
	}

	/**
	 * 返回 **你谷** 主页源代码
	 */
	val homePage : HttpEntity get() = getRequest().run(::execute).takeIf { it.statusLine.statusCode == 200 }?.entity ?: throw LuoGuException(this, "wrong status code")

	/**
	 * 一个奇怪的Token, 似乎十分重要, 大部分操作都需要这个
	 */
	val csrfToken : String
		get() {
			getRequest().let { req ->
				client.execute(req) !!.let { resp ->
					return csrfToken(Jsoup.parse(EntityUtils.toString(resp.entity))) ?: throw LuoGuException(this, "No such csrf-token")
				}
			}
		}

	val sliderPhotos : List<Pair<String, String>>
		get() {
			return getRequest().run(client::execute).entity.data.run(Jsoup::parse).run(LuoGu.Companion::sliderPhotos)
		}

	val practiceList : List<PracticeBlock>
		get() {
			TODO()
		}

	/**
	 * 获得当前客户端登录的用户
	 */
	@get:Throws(StatusCodeException::class, LuoGuException::class)
	val loggedUser : LuoGuLoggedUser get() = LuoGuLoggedUser(this)

	/**
	 * 获取验证码
	 * @param output 输出流, 将会把验证码**图片**输出到这个流里
	 */
	fun verifyCode(output : OutputStream) {
		getRequest("download/captcha").let { req ->
			execute(req) !!.let { resp ->
				val statusCode = resp.statusLine.statusCode
				val content : ByteArray = EntityUtils.toByteArray(resp.entity)
				if (statusCode == 200) {
					output.write(content)
				} else throw LuoGuStatusCodeException(this, statusCode, String(content))
			}
		}
	}

	/**
	 * 登录**你谷**
	 * @param account 账号
	 * @param password 密码
	 * @param verifyCode 验证码, 通过 LuoGu::verifyCode 获得
	 * @throws APIStatusCodeException 当登录失败时抛出
	 * @throws StatusCodeException 当请求码错误时抛出
	 * @return 返回一个 LuoGuLoginResule 对象
	 *
	 * @see LuoGu.verifyCode
	 * @see LuoGuLoggedUser
	 * @see APIStatusCodeException
	 * @see StatusCodeException
	 */
	@Throws(StatusCodeException::class, APIStatusCodeException::class)
	fun login(account : String, password : String, verifyCode : String) : LuoGuLoggedUser {
		return postRequest("login/loginpage").apply {
			val cookie = 0
			val redirect = ""
			val twoFactor = "undefined"

			entity = mapOf(
					"username" to account,
					"password" to password,
					"cookie" to cookie.toString(),
					"redirect" to redirect,
					"two_factor" to twoFactor,
					"verify" to verifyCode
			).stringEntity()
		}.let { req ->
			execute(req) !!.let { resp ->
				val statusCode = resp.statusLine.statusCode
				val content : String = EntityUtils.toString(resp.entity)
				if (statusCode == 200) {
					JSONObject(content).run {
						val code : Int = getInt("code")
						val msg : String = getString("message")

						if (code == 200) {
							loggedUser
						} else throw APIStatusCodeException(code, msg)
					}
				} else throw StatusCodeException(statusCode)
			}
		}
	}

	/**
	 * 公开的犇犇列表
	 * @param page 页面序号
	 * @return 返回一个评论列表
	 * @see LuoGuComment
	 */
	@JvmOverloads
	fun publicBenben(page : Int = 1) : List<LuoGuComment> = LuoGuLoggedUser(this, "Internal").benben(BenBenType.ALL, page)

	/**
	 * 题目列表
	 * @param page 页数, 默认为 **1**
	 * @param filter 过滤器
	 * @throws StatusCodeException
	 * @return 返回题目列表
	 *
	 * @see Problem
	 * @see ProblemSearchConfig
	 */
	@JvmOverloads
	@Throws(StatusCodeException::class)
	fun problemList(page : Int = 1, filter : ProblemSearchConfig = ProblemSearchConfig()) : List<Problem> {
		getRequest("problemnew/lists?$filter&page=$page").run(::execute).let { resp ->
			val statusCode = resp.statusLine.statusCode
			val content = resp.entity.data

			if (statusCode == 200) {
				return ProblemListPage(Jsoup.parse(content)).list()
			} else throw StatusCodeException(statusCode, content)
		}
	}
}