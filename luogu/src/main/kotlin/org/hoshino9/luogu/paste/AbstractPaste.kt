package org.hoshino9.luogu.paste

import org.hoshino9.luogu.LuoGuUtils

abstract class AbstractPaste : Paste {
	override val url : String = run { "${LuoGuUtils.baseUrl}/paste/$id" }
}