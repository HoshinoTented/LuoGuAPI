package org.hoshino9.luogu.training

import com.google.gson.JsonArray
import com.google.gson.JsonNull
import com.google.gson.JsonObject
import io.ktor.client.call.receive
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.user.ProblemID
import org.hoshino9.luogu.utils.*

sealed class TrainingForm(val title: String, val description: String, val type: Int) {
	class PersonalPublic(title: String, description: String) : TrainingForm(title, description, 5)
	class PersonalPrivate(title: String, description: String) : TrainingForm(title, description, 8)

	sealed class GroupForm(title: String, description: String, type: Int, val teamID: Int) : TrainingForm(title, description, type) {
		class GroupPublic(title: String, description: String, teamID: Int) : GroupForm(title, description, 7, teamID)
		class GroupPrivate(title: String, description: String, teamID: Int) : GroupForm(title, description, 2, teamID)
		/**
		 * @param deadline **秒级** 时间戳
		 */
		class GroupHomework(title: String, description: String, teamID: Int, val deadline: Long) : GroupForm(title, description, 4, teamID)

		override fun asJson(): JsonObject {
			return super.asJson().apply {
				addProperty("providerID", teamID)
			}
		}
	}


	open fun asJson(): JsonObject = run {
		JsonObject().apply {
			add("settings", JsonObject().apply {
				addProperty("title", title)
				addProperty("description", description)
				addProperty("type", type)
			})
		}
	}
}

internal suspend fun LuoGu.modifyTraining(mode: String, form: TrainingForm): Int = run {
	apiPost("api/training/$mode") {
		this.body = form.asJson().asParams
		referer("")
	}.receive<String>()
			.apply(::println)
			.run(::json)
			.get("id").asInt
}

suspend fun LuoGu.deleteTraining(id: Int) {
	apiPost("api/training/delete/$id") {
		referer("")
	}.receive<String>()
}

suspend fun LuoGu.newTraining(form: TrainingForm): Int = modifyTraining("new", form)
suspend fun LuoGu.editTraining(form: TrainingForm): Int = modifyTraining("edit", form)

suspend fun LuoGu.editTrainingProblems(id: Int, problems: List<ProblemID>) {
	apiPost("api/training/editProblems/$id") {
		body = JsonObject().apply {
			add("pids", JsonArray().apply {
				problems.forEach {
					add(it)
				}
			})
		}.asParams

		referer("")
	}.receive<String>()
}