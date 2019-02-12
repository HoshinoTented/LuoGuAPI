@file:Suppress("MemberVisibilityCanBePrivate")

import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.benben.BenBenType
import org.hoshino9.luogu.comment.Comment
import org.hoshino9.luogu.discuss.DiscussListPage
import org.hoshino9.luogu.problem.ProblemContent
import org.hoshino9.luogu.problem.ProblemFromId
import org.hoshino9.luogu.results.SignedInStatus
import org.hoshino9.luogu.user.HasBadgeUser
import org.junit.Before
import org.junit.Test
import java.io.FileOutputStream
import java.nio.file.Files
import java.nio.file.Paths
import java.util.Properties
import java.util.Scanner

class LuoGuTest {
	companion object {
		private val testRoot = Paths.get("src", "test", "resources")
		private val verifyPath by lazy { testRoot.resolve("verify.png") }
		private val configPath by lazy { testRoot.resolve("user.properties") }
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

	fun login() {
		luogu = LuoGu()
		luogu.verifyCode(Files.newOutputStream(verifyPath))
		println("Please input verify code")
		val verifyCode : String = Scanner(System.`in`).next()
		luogu.login(config.getProperty("account"), config.getProperty("password"), verifyCode)
	}

	fun saveCookie() {
		config.setProperty("__client_id", luogu.clientId)
		config.setProperty("_uid", luogu.myuid)
		config.store(Files.newOutputStream(configPath), null)
	}

	@Test
	fun signInTest() {
		val toString : (SignedInStatus.Thing) -> String = {
			"${it.name}: ${it.description}"
		}

		val status = try {
			user.signInStatus
		} catch (e : IllegalStateException) {
			println("failed, trying signing in...")
			user.signIn()
			user.signInStatus
		}

		//language=TEXT
		"""${status.qian.show}
宜:
${status.goods.joinToString(separator = "\n", transform = toString)}

忌:
${status.bads.joinToString(separator = "\n", transform = toString)}
""".run(::println)

	}

	@Test
	fun photoListTest() {
		user.photoList().joinToString {
			//language=TEXT
			"""user: ${it.user}
url: ${it.url}
date: ${it.date}
${it.user}
"""
		}.run(::println)
	}

	//	@Test
	@Suppress("DEPRECATION")
	fun benbenTest() {
		val toString : (Comment) -> String = {
			val user = it.user

			//language=TEXT
			"""user: ${if (user is HasBadgeUser) "${user.uid}[${user.badge.text}]" else user.uid}
date: ${it.date}
content:
${it.content}
"""
		}

		user.getBenben(BenBenType.ALL).joinToString(separator = separator, transform = toString).run(::println)
		user.getBenben(BenBenType.WATCHING).joinToString(separator = separator, transform = toString).run(::println)
	}

	@Test
	fun pasteListTest() {
		user.pasteList().joinToString(separator = separator) {
			//language=TEXT
			"""user: ${it.user}
date: ${it.date}
is public: ${it.isPublic}
source:
${it.source}
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
		luogu.problemList().forEach {
			println("${it.name}(${it.id}) ${it.tags} (${it.passPercent.first} / ${it.passPercent.second})")
			ProblemFromId(it.id, luogu.client).let { p ->
				println("${p.name}(${p.id}) ${p.tags}(${p.difficulty}) (${p.passPercent.first} / ${p.passPercent.second})")
			}
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
					println(problem.id)
				}
			}

			println(separator)
		}
	}

	@Test
	fun userTest() {
		println("$user: ${user.spacePage.username}")
		user.spacePage.passedProblems.run(::println)
		user.spacePage.triedProblems.run(::println)
		user.spacePage.gugugu.run(::println)
	}

	//	@Test
	fun paste() {
		"LuoGu API Test".let { content ->
			user.postPaste(content).let { paste ->
				println(paste.source)
				user.deletePaste(paste)
			}
		}
	}

	@Test
	fun discuss() {
		DiscussListPage("", client = luogu.client).let { list ->
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
	fun problemContent() {
		ProblemContent.parse("P1001").run(::println)
	}

	@Test
	fun posts() {
		luogu.posts.run(::println)
	}
}