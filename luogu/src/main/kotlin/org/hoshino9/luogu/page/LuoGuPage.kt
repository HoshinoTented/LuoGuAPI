package org.hoshino9.luogu.page

import org.json.JSONObject
import org.jsoup.nodes.Document

interface LuoGuPage {
	val url: String
	val feInjection: JSONObject
}