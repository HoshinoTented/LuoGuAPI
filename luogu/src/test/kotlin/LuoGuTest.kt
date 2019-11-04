@file:Suppress("MemberVisibilityCanBePrivate")

import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.discuss.DiscussListPage
import org.hoshino9.luogu.discuss.posts
import org.hoshino9.luogu.paste.Paste
import org.hoshino9.luogu.paste.deletePaste
import org.hoshino9.luogu.paste.pasteList
import org.hoshino9.luogu.paste.postPaste
import org.hoshino9.luogu.photo.photoList
import org.hoshino9.luogu.problem.Problem
import org.hoshino9.luogu.problem.problemList
import org.hoshino9.luogu.training.trainingPage
import org.hoshino9.luogu.user.LoggedUser
import org.junit.Before
import org.junit.Test
import java.nio.file.Files
import java.nio.file.Paths
import java.util.Properties

open class LuoGuTest {
	companion object {
		internal val testRoot = Paths.get("testResources")
		internal val verifyPath by lazy { testRoot.resolve("verify.png") }
		internal val configPath by lazy { testRoot.resolve("user.properties") }
		internal val config by lazy {
			Properties().apply {
				load(Files.newInputStream(configPath))
			}
		}
	}

	lateinit var luogu : LuoGu

	val user by lazy { this.luogu.loggedUser }

	val separator = "${"=".repeat(100)}\n"

	@Before
	fun loadCookie() {
		val id : String? = config.getProperty("__client_id")
		val uid : String? = config.getProperty("_uid")

		if (id != null && uid != null) {
			luogu = LuoGu(id, uid)
		} else throw IllegalStateException("No logged in")

	}

	fun saveCookie() {
		config.setProperty("__client_id", luogu.clientId)
		config.setProperty("_uid", luogu.uid)
		config.store(Files.newOutputStream(configPath), null)
	}

	@Test
	fun photoListTest() {
		user.photoList(1).joinToString {
			val it = it.newInstance()

			//language=TEXT
			"""user: ${it.user}
url: ${it.url}
date: ${it.date}
${it.user}
"""
		}.run(::println)
	}

	//	@Test
//	@Suppress("DEPRECATION")
//	fun benbenTest() {
//		val toString : (Comment) -> String = {
//			val user = it.user
//
//			//language=TEXT
//			"""user: ${if (user is HasBadgeUser) "${user.uid}[${user.badge.text}]" else user.uid}
//date: ${it.date}
//content:
//${it.content}
//"""
//		}
//
//		user.getBenben(BenBenType.ALL).joinToString(separator = separator, transform = toString).run(::println)
//		user.getBenben(BenBenType.WATCHING).joinToString(separator = separator, transform = toString).run(::println)
//	}

	@Test
	fun pasteListTest() {
		user.pasteList().list.joinToString(separator = separator) {
			//language=TEXT
			"""user: ${it.user}
date: ${it.time}
is public: ${it.isPublic}
source:
${it.data}
"""
		}.run(::println)
	}

	@Test
	fun sliderPhotoTest() {
		luogu.sliderPhotos.forEach {
			println(it)
//			val time = measureTimeMillis {
//				defaultClient.executeGet(it.img) { resp ->
//					resp.assert()
//					resp.body() !!.byteStream().copyTo(testRoot.resolve(it.img.hashCode().toString() + ".png").toFile().outputStream())
//				}
//			}
//
//			println("used $time ms")
		}
	}

	@Test
	fun problemListTest() {
		luogu.problemList().result.map { Problem.Factory(it, luogu.client) }.forEach {
			println("${it.title}(${it.pid})[${it.difficulty}] ${it.tags} (${it.totalAccepted} / ${it.totalSubmit})")
		}
	}

	@Test
	fun training() {
		luogu.trainingPage.also {
			println("passed: ${it.passedCount}")
			println("newest passed: ${it.lastPassedBlock}")
			println("skip: ${it.skipPercent}")
		}.trainingBlocks.forEach {
			println(it.name)
			it.trainings.forEach { training ->
				print("[${training.status.content}]")
				println(training.name)
				training.problems.forEach { problem ->
					println(problem.pid)
				}
			}

			println(separator)
		}
	}

	@Test
	fun userTest() {
		println("$user: ${user.name}")
		user.passedProblems.run(::println)
		user.submittedProblems.run(::println)
	}

	//	@Test
	fun paste() {
		"LuoGu API Test".let { content ->
			user.postPaste(content).let { paste ->
				println(Paste.PastePage(paste, luogu.client).newInstance().data)
				user.deletePaste(paste)
			}
		}
	}

	@Test
	fun discuss() {
		DiscussListPage.Factory("", 1, luogu.client).let { list ->
			list.discusses.first().infoPage.let { info ->
				info.mainComment.run {
					"""User: $user
Date: $date
Content: $content
					""".run(::println)
				}
			}
		}
	}

	@Test
	fun posts() {
		luogu.posts.run(::println)
	}
}