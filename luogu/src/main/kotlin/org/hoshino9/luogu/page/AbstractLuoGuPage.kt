package org.hoshino9.luogu.page

import org.hoshino9.luogu.utils.json
import org.json.JSONObject
import java.net.URLDecoder

abstract class AbstractLuoGuPage : LuoGuPage {
	companion object {
		private val regex = Regex("""window\._feInjection = JSON\.parse\(decodeURIComponent\("(.+?)"\)\);""")
	}

	override val feInjection : JSONObject by lazy {
		json(URLDecoder.decode(regex.find(page.toString()) !!.groupValues[1], "UTF-8"))
	}
}