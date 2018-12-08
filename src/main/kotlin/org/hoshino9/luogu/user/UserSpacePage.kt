@file:Suppress("unused", "MemberVisibilityCanBePrivate")

package org.hoshino9.luogu.user

import okhttp3.OkHttpClient
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.problem.Problem
import org.hoshino9.luogu.problem.ProblemFromId
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.select.Elements

open class UserSpacePage(val user : User, val client : OkHttpClient = defaultClient) : HasElement {
	override val elem : Element by lazy {
		client.getExecute("$baseUrl/space/show?uid=${user.uid}") { resp ->
			resp.assert()
			resp.data !!.run(Jsoup::parse).body()
		}
	}

	private val rightContent : Element by lazy {
		elem.getElementsByClass("am-u-md-4 lg-right").first()
	}

	/**
	 * 用户名
	 */
	val username : String by lazy {
		"lg-toolbar".let { className ->
			elem.getElementsByClass(className).first().child(0).text().let { text ->
				text.substring(text.indexOf(' ') + 1)
			}
		}
	}

	/**
	 * 用户的签名
	 * **Nullable**
	 */
	val introduction : Element by lazy {
		TODO()
	}

	/**
	 * 通过的题目
	 * 返回一个 List
	 * 题目的pid
	 */
	val passedProblems : List<Problem> by lazy {
		parseProblems(rightContent.child(2).children())
	}

	/**
	 * 尝试过的题目
	 * 返回一个 List
	 * 题目的pid
	 */
	val triedProblems : List<Problem> by lazy {
		parseProblems(rightContent.child(4).children())
	}

	/**
	 * 咕值
	 */
	val gugugu : Int by lazy { TODO() }

	val avatar : String get() = "https:://cdn.luogu.org/upload/usericon/${user.uid}.png"

	private fun parseProblems(es : Elements) : List<Problem> {
		return es.mapNotNull { elem ->
			elem.takeIf { it.tagName() == "a" }?.text()?.let { ProblemFromId(it, client) }
		}
	}
}